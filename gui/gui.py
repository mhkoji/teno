import gi
gi.require_version("Gtk", "3.0")
gi.require_version('PangoCairo', '1.0')

from gi.repository import Gtk, PangoCairo, Pango

WINDOW_TITLE = "teno"

class Memo:
    def __init__(self, text):
        self.text = text


class Window(Gtk.Window):
    def __init__(self):
        super().__init__(title=WINDOW_TITLE)

        self.set_default_size(640, -1)

        box = Gtk.Box()
        box.set_orientation(Gtk.Orientation.VERTICAL)
        self.add(box)

        self.entry = Gtk.Entry()
        box.add(self.entry)

        self.memo_box = Gtk.Box()
        self.memo_box.set_orientation(Gtk.Orientation.VERTICAL)
        box.add(self.memo_box)

        self.draw_memos([
            Memo(text="aaa"),
            Memo(text="bbb"),
            Memo(text="ccc"),
        ])

    def draw_memos(self, memos):
        for child in self.memo_box.get_children():
            child.destroy()

        for memo in memos:
            label = Gtk.Label()
            label.set_text(memo.text)
            self.memo_box.add(label)


def main():
    win = Window()
    win.connect("destroy", Gtk.main_quit)
    win.show_all()
    Gtk.main()

if __name__ == '__main__':
    main()
