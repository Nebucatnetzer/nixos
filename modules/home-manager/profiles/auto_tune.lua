local dt = require "darktable"

----------------------------------------------------------------------
-- Debug log: writes to stdout (visible if darktable is started with
-- `-d lua` or from a terminal) AND shows a brief toast in the GUI.
----------------------------------------------------------------------

local function log(msg)
  print("[auto_tune] " .. msg)
  dt.print(msg)
end

----------------------------------------------------------------------
-- Event wait
----------------------------------------------------------------------

local wait_flag = nil

local function on_wait_event(event, image)
  wait_flag = true
end

local function wait_for_event(event_name, max_ms, fn)
  log("waiting for " .. event_name)
  wait_flag = nil
  dt.register_event("auto_tune_wait", event_name, on_wait_event)
  fn()

  local period = 250
  local elapsed = 0
  -- Wait until the event fires AND at least one full poll period has elapsed.
  -- The one-period minimum filters stale pixelpipe events from the previous
  -- step that would otherwise be captured before our action has been processed.
  while (not wait_flag) or (elapsed < period) do
    dt.control.sleep(period)
    elapsed = elapsed + period
    if elapsed >= max_ms then break end
  end

  dt.destroy_event("auto_tune_wait", event_name)
  -- Drain any residual renders queued by this action. With the listener
  -- gone, events that fire during this sleep are silently discarded and
  -- cannot trigger the next step's wait prematurely.
  dt.control.sleep(period)

  if wait_flag then
    log(event_name .. " fired after " .. elapsed .. " ms")
  else
    log(event_name .. " TIMEOUT after " .. elapsed .. " ms")
  end
end

----------------------------------------------------------------------
-- Exposure picker
----------------------------------------------------------------------

local function ensure_exposure_enabled()
  log("ensure_exposure_enabled: enable")
  dt.gui.action("iop/exposure", 0, "enable", "", 1.0)
  log("ensure_exposure_enabled: show")
  dt.gui.action("iop/exposure", 0, "show", "", 1.0)
end

local function run_exposure_picker()
  ensure_exposure_enabled()

  wait_for_event("pixelpipe-processing-complete", 3000, function()
    log("exposure picker: toggle 1 (activate)")
    dt.gui.action("iop/exposure/exposure", 0, "button", "toggle", 1.0)
  end)

  wait_for_event("pixelpipe-processing-complete", 3000, function()
    log("exposure picker: toggle 2 (release)")
    dt.gui.action("iop/exposure/exposure", 0, "button", "toggle", 1.0)
  end)
end

----------------------------------------------------------------------
-- Color calibration picker
----------------------------------------------------------------------

local function ensure_channelmixerrgb_enabled()
  log("ensure_channelmixerrgb_enabled: enable")
  dt.gui.action("iop/channelmixerrgb", 0, "enable", "", 1.0)
  log("ensure_channelmixerrgb_enabled: show")
  dt.gui.action("iop/channelmixerrgb", 0, "show", "", 1.0)
end

local function run_color_calibration_picker()
  ensure_channelmixerrgb_enabled()

  wait_for_event("pixelpipe-processing-complete", 3000, function()
    log("color calibration picker: toggle 1 (activate)")
    dt.gui.action("iop/channelmixerrgb/picker", 0, "", "toggle", 1.0)
  end)

  wait_for_event("pixelpipe-processing-complete", 3000, function()
    log("color calibration picker: toggle 2 (release)")
    dt.gui.action("iop/channelmixerrgb/picker", 0, "", "toggle", 1.0)
  end)
end

local function load_image(image)
  if dt.gui.current_view().id ~= "darkroom" then
    log("load_image: switch to darkroom")
    -- current_view() is async; wait for view-changed before touching any
    -- iop actions, otherwise they fire while still in lighttable and fail
    -- with "action not valid for current view".
    wait_for_event("view-changed", 3000, function()
      dt.gui.current_view(dt.gui.views.darkroom)
    end)
  end

  -- Calling display_image on an already-displayed image reloads it from the
  -- database, discarding in-memory history (including auto-apply presets not
  -- yet flushed to DB). Only call it when we actually need to switch images.
  local visible = dt.gui.action_images
  if visible and #visible > 0 and visible[1] == image then
    log("load_image: already displayed, skipping display_image")
    return
  end

  wait_for_event("darkroom-image-loaded", 3000, function()
    log("load_image: display_image")
    dt.gui.views.darkroom.display_image(image)
  end)
end

----------------------------------------------------------------------
-- AgX pickers
----------------------------------------------------------------------

local function ensure_agx_enabled()
  log("ensure_agx_enabled: enable")
  dt.gui.action("iop/agx", 0, "enable", "", 1.0)
  log("ensure_agx_enabled: show")
  dt.gui.action("iop/agx", 0, "show", "", 1.0)
end

local function run_agx_pickers()
  ensure_agx_enabled()

  -- Single toggle: this button is momentary and self-resets after sampling.
  wait_for_event("pixelpipe-processing-complete", 3000, function()
    log("agx auto tune levels: toggle")
    dt.gui.action("iop/agx/exposure range/auto tune levels", 0, "", "toggle", 1.0)
  end)

  wait_for_event("pixelpipe-processing-complete", 3000, function()
    log("agx pivot target output: toggle 1 (activate)")
    dt.gui.action("iop/agx/curve/pivot target output", 0, "button", "toggle", 1.0)
  end)

  wait_for_event("pixelpipe-processing-complete", 3000, function()
    log("agx pivot target output: toggle 2 (release)")
    dt.gui.action("iop/agx/curve/pivot target output", 0, "button", "toggle", 1.0)
  end)
end

local function process_image(image)
  load_image(image)
  run_exposure_picker()
  run_color_calibration_picker()
  run_agx_pickers()
end

----------------------------------------------------------------------
-- Entry point
----------------------------------------------------------------------

local function run()
  log("run: button clicked")
  local selected = dt.gui.action_images
  if not selected or #selected == 0 then
    log("run: no images selected")
    return
  end
  log("run: " .. #selected .. " images selected")

  for index, image in ipairs(selected) do
    log(string.format("processing image %d of %d", index, #selected))
    process_image(image)
  end

  log(string.format("done: %d images", #selected))
end

dt.register_lib(
  "auto_tune_lib",
  "Auto Tune",
  true, false,
  {[dt.gui.views.lighttable] = {"DT_UI_CONTAINER_PANEL_RIGHT_CENTER", 0}},
  dt.new_widget("button") {
    label = "Auto-tune selection",
    clicked_callback = run
  }
)
