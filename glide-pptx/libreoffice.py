# Driving a running LibreOffice, for `raco glide`.
#
# LibreOffice has no reload on its command line: opening a file that is already
# open raises the window it is already showing, whatever the file on disk says
# now. A deck is regenerated every time the program is saved, so without this
# the editor shows the deck from several edits ago -- and saving from there
# writes that back over the program's work.
#
# So it is asked over UNO, which is the interface it does have. `soffice` is
# started with a socket and this connects to it.
#
#   reload <port> <path>   reload that document, keeping the slide in view
#   open <port> <path>     exit 0 if that document is still open
#
# Exit codes say what happened: 0 did it, 3 could not connect, 4 no such
# document open. Anything else is a failure to report.
import sys
import time

def connect(port):
    import uno
    local = uno.getComponentContext()
    resolver = local.ServiceManager.createInstanceWithContext(
        "com.sun.star.bridge.UnoUrlResolver", local)
    return resolver.resolve(
        "uno:socket,host=localhost,port=%s;urp;StarOffice.ComponentContext" % port)

def find(ctx, url):
    # A document that has been closed stays in the enumeration for a while and
    # answers everything with DisposedException, so it is asked for its pages
    # before being believed.
    desktop = ctx.ServiceManager.createInstanceWithContext(
        "com.sun.star.frame.Desktop", ctx)
    docs = desktop.getComponents().createEnumeration()
    while docs.hasMoreElements():
        d = docs.nextElement()
        try:
            if d.getURL() == url:
                d.getDrawPages().getCount()
                return d
        except Exception:
            pass
    return None

def main(argv):
    if len(argv) != 4:
        sys.stderr.write("usage: libreoffice.py reload|open <port> <path>\n")
        return 2
    what, port, path = argv[1], argv[2], argv[3]
    import unohelper
    url = unohelper.systemPathToFileUrl(path)
    try:
        ctx = connect(port)
    except Exception:
        return 3
    doc = find(ctx, url)
    if doc is None:
        return 4
    if what == "open":
        return 0
    # The slide being looked at, so the reload does not send the editor back to
    # the first one. A regeneration happens once a keystroke.
    page = None
    try:
        page = doc.getCurrentController().getCurrentPage()
    except Exception:
        pass
    index = None
    if page is not None:
        try:
            pages = doc.getDrawPages()
            for i in range(pages.getCount()):
                if pages.getByIndex(i) == page:
                    index = i
                    break
        except Exception:
            pass
    # Asked for from the frame itself. The DispatchHelper takes the same
    # command and does nothing with it, without saying so.
    from com.sun.star.util import URL as UnoURL
    trans = ctx.ServiceManager.createInstanceWithContext(
        "com.sun.star.util.URLTransformer", ctx)
    command = UnoURL()
    command.Complete = ".uno:Reload"
    _, command = trans.parseStrict(command)
    frame = doc.getCurrentController().getFrame()
    dispatch = frame.queryDispatch(command, "_self", 0)
    if dispatch is None:
        return 5
    dispatch.dispatch(command, ())
    # Waited for, and not only so the slide can be put back. The dispatch is
    # asynchronous and the bridge is what carries it: a process that asks and
    # exits takes the connection with it, and the reload never happens.
    fresh = None
    for _ in range(60):
        time.sleep(0.25)
        candidate = find(ctx, url)
        if candidate is not None and candidate != doc:
            fresh = candidate
            break
    if fresh is None:
        return 5
    if index is not None:
        try:
            pages = fresh.getDrawPages()
            if index < pages.getCount():
                fresh.getCurrentController().setCurrentPage(pages.getByIndex(index))
        except Exception:
            pass
    return 0

if __name__ == "__main__":
    sys.exit(main(sys.argv))
