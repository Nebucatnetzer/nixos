-- mpv saves the position only on quit, not when it changes to another file.
-- This script also saves it when a file is left before its end, for example with playlist-next.
mp.add_hook("on_unload", 50, function()
    -- Only act for the players that resume, such as watch-random-video.
    if not mp.get_property_bool("save-position-on-quit") then
        return
    end
    -- A file that played to its end starts from the beginning next time.
    if mp.get_property_bool("eof-reached") then
        return
    end
    mp.command("write-watch-later-config")
end)
