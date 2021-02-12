import dash
from webapp.core import RenderableRegistry

__APP__ = None

class Application:
    @classmethod
    def init(cls):
        external_stylesheets = [
            "https://cdn.jsdelivr.net/npm/semantic-ui@2.4.2/dist/semantic.min.css"
        ]

        external_scripts = [
            "https://code.jquery.com/jquery-3.1.1.min.js",
            "https://cdn.jsdelivr.net/npm/semantic-ui@2.4.2/dist/semantic.min.js",
        ]

        application = dash.Dash(
            __name__,
            external_stylesheets=external_stylesheets,
            external_scripts=external_scripts,
            suppress_callback_exceptions=True
        )

        __APP__ = application
        return __APP__, __APP__.server

    @classmethod
    def register_callbacks(cls, app):
        for renderable in RenderableRegistry.RENDERABLES:
            renderable.register_callbacks(app)
