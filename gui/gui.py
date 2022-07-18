import gi
gi.require_version("Gtk", "3.0")
gi.require_version('PangoCairo', '1.0')

from gi.repository import Gtk, PangoCairo, Pango, GLib

import websocket, threading
import json

WINDOW_TITLE = "teno"

class Memo:
    def __init__(self, text):
        self.text = text


class Service:
    def __init__(self,
                 on_ready=None,
                 on_draw_memos=None):
        self.ws = websocket.WebSocketApp(
            "ws://localhost:5000",
            on_open=self.on_open,
            on_message=self.on_message)
        self.on_ready = on_ready
        self.on_draw_memos = on_draw_memos

    def on_open(self, ws):
        GLib.idle_add(self.on_ready)

    def on_message(self, ws, msg):
        msg_json = json.loads(msg)

        op = msg_json["op"]
        if op == "draw_memos":
            memos = [Memo(memo_json["text"])
                     for memo_json in msg_json["memos"]]
            GLib.idle_add(self.on_draw_memos, memos)
        else:
            print("Unknown msg: {}".format(msg))

    def list_memos(self):
        self.send(json.dumps({
            'op': 'list_memos'
        }))

    def send(self, msg):
        self.ws.send(msg)


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

        self.service = Service(
            on_ready=self.on_ready,
            on_draw_memos=self.draw_memos)
        th = threading.Thread(target=self.service.ws.run_forever)
        th.daemon = True
        th.start()

    def on_ready(self):
        self.service.list_memos()

    def draw_memos(self, memos):
        for child in self.memo_box.get_children():
            child.destroy()

        for memo in memos:
            label = Gtk.Label()
            label.set_text(memo.text)
            self.memo_box.add(label)

        self.memo_box.show_all()


def main():
    # websocket.enableTrace(True)
    win = Window()
    win.connect("destroy", Gtk.main_quit)
    win.show_all()
    Gtk.main()


if __name__ == '__main__':
    main()
