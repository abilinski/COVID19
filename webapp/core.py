import abc

class RenderableRegistry(object):
    RENDERABLES = []

    @classmethod
    def register(cls, renderable):
        cls.RENDERABLES.append(renderable)


class Renderable(object, metaclass=abc.ABCMeta):
    def __init__(self):
        RenderableRegistry.register(self)

    def render(self):
        raise NotImplementedError()

    def register_callbacks(self):
        raise NotImplementedError()
