#!/usr/bin/env python
import dbus

try:
    session_bus = dbus.SessionBus()
    spotify_bus = session_bus.get_object("org.mpris.MediaPlayer2.spotify",
                                         "/org/mpris/MediaPlayer2")
    spotify_properties = dbus.Interface(spotify_bus,
                                        "org.freedesktop.DBus.Properties")
    metadata = spotify_properties.Get("org.mpris.MediaPlayer2.Player", "Metadata")

    title = metadata['xesam:title']
    artists = metadata['xesam:artist']
    artists_str = ", ".join(artists) if len(artists) > 1 else artists[0]

    # print(artists)
    print("%s | %s" % (title, artists_str))
except:
    print("Not playing")
