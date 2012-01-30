from twisted.application         import strports
from twisted.application.service import Application
from twisted.internet            import reactor
from twisted.internet.protocol   import Factory, Protocol
from twisted.web.static          import File
from twisted.web.server          import Site
from txws                        import WebSocketFactory


class EchoUpper(Protocol):
    """Echo uppercased."""
    def dataReceived(self, data):
        log.msg("Got %r" % (data,))
        self.transport.write(data.upper())


if __name__ == '__main__':
    application = Application("ws-streamer")

    echofactory = Factory()
    echofactory.protocol = EchoUpper
    service = strports.service("tcp:8076:interface=127.0.0.1",
                               WebSocketFactory(echofactory))
    service.setServiceParent(application)

    resource = File("./public")
    webservice = strports.service("tcp:8080:interface=127.0.0.1",
                                  Site(resource))
    webservice.setServiceParent(application)


