# Drive a running LibreOffice to edit the deck it has open, then save it.
# This is the editor doing the editing, rather than a test writing the XML it
# imagines an editor writes.
import sys, time, uno, unohelper
from com.sun.star.awt import Point, Size

def connect(port):
    local = uno.getComponentContext()
    r = local.ServiceManager.createInstanceWithContext(
        "com.sun.star.bridge.UnoUrlResolver", local)
    return r.resolve("uno:socket,host=localhost,port=%s;urp;StarOffice.ComponentContext" % port)

def doc_for(ctx, url):
    desktop = ctx.ServiceManager.createInstanceWithContext("com.sun.star.frame.Desktop", ctx)
    e = desktop.getComponents().createEnumeration()
    while e.hasMoreElements():
        d = e.nextElement()
        try:
            if d.getURL() == url:
                d.getDrawPages().getCount()
                return d
        except Exception:
            pass
    return None

def main(argv):
    what, port, path = argv[1], argv[2], argv[3]
    url = unohelper.systemPathToFileUrl(path)
    ctx = connect(port)
    doc = doc_for(ctx, url)
    if doc is None:
        return 4
    page = doc.getDrawPages().getByIndex(0)
    if what == "draw":
        shape = doc.createInstance("com.sun.star.drawing.RectangleShape")
        page.add(shape)
        shape.setPosition(Point(4000, 3000))
        shape.setSize(Size(3000, 2000))
    elif what == "move":
        s = page.getByIndex(0)
        p = s.getPosition()
        s.setPosition(Point(p.X + 2000, p.Y + 1500))
    elif what == "retext":
        for i in range(page.getCount()):
            s = page.getByIndex(i)
            if hasattr(s, "getString") and s.getString().strip():
                s.setString("edited in libreoffice")
                break
    elif what == "touch":
        pass
    elif what == "recolor":
        for i in range(page.getCount()):
            s = page.getByIndex(i)
            if hasattr(s, "FillColor"):
                s.FillColor = 0x70AD47
                break
    doc.store()
    time.sleep(0.5)
    return 0

if __name__ == "__main__":
    sys.exit(main(sys.argv))
