from tornado.websocket import WebSocketHandler
from tornado.httpserver import HTTPServer
from tornado.ioloop import IOLoop
from tornado.web import Application


class Handler(WebSocketHandler):
    def open(self):
        print "open websocket"

    def on_message(self, message):
        self.write_message(u"You said: " + message)

    def on_close(self):
        print "websocket closed"


HTTPServer(Application([("/", Handler)])).listen(1024)
IOLoop.instance().start()
