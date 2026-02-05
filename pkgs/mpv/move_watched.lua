local utils = require "mp.utils"

-- Initialize the list of files to be moved
local move_list = {}

-- Helper function to ensure the target 'watched' directory exists
function ensure_dir(path)
    if not utils.file_info(path) then
        -- Creates the directory (and parent directories if needed) [cite: 1]
        os.execute(string.format('mkdir -p "%s"', path))
    end
end

-- Function to mark the current file for moving [cite: 3]
function mark_for_move()
    local work_dir = mp.get_property_native("working-directory")
    local file_path = mp.get_property_native("path")

    -- Construct the absolute path [cite: 3]
    local final_path
    local s = file_path:find(work_dir, 0, true)
    if s and s == 0 then
        final_path = file_path
    else
        final_path = utils.join_path(work_dir, file_path)
    end

    -- Add to the move list [cite: 3]
    table.insert(move_list, final_path)
    mp.osd_message("Marked for watched folder")
end

-- Function to execute the move on shutdown
function move_files()
    for _, filepath in pairs(move_list) do
        local dir, filename = utils.split_path(filepath)

        -- Logic: If path contains 'playlist/', move to subdirectory/watched/
        -- Otherwise, move to a generic watched folder in the parent directory
        local target_dir
        if dir:find("playlist/") then
            target_dir = utils.join_path(dir, "watched")
        else
            -- Default to ~/Downloads/youtube/watched logic
            target_dir = utils.join_path(dir, "watched")
        end

        ensure_dir(target_dir)
        local dest_path = utils.join_path(target_dir, filename)

        print("Moving: " .. filename .. " -> " .. target_dir)
        os.rename(filepath, dest_path)
    end
end

-- Keybinding: Only Ctrl+m is needed now
mp.add_key_binding("ctrl+m", "mark_move", mark_for_move)

-- Run the move process when MPV closes
mp.register_event("shutdown", move_files)
