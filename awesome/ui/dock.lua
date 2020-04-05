local awful = require('awful')
local gfs = require('gears.filesystem')
local wibox = require('wibox')

local home = gfs.get_configuration_dir()
local dock_dir = home .. "dock/"
local icon_dir = "/usr/share/icons/hicolor/32x32/apps/"

local dock = {}

-- Loop through the `dock_dir` and find apps to put in the dock.
local function discoverApps()
    local files = io.popen('ls -1 ' .. dock_dir)
    local apps = {}
    for f in files:lines() do
        local icon = io.popen("grep Icon " .. dock_dir .. f .. " | cut -d '=' -f 2"):lines()
        local command = "firefox"
        table.insert(apps, {icon = icon_dir .. icon() .. '.png', command = command})
    end

    return apps
end

-- Create a dock to go on the given screen.
local function create(screen)
    local ff = "/usr/share/icons/hicolor/32x32/apps/firefox.png"
    local apps = {layout = wibox.layout.fixed.vertical}

    for idx, app in ipairs(discoverApps()) do
        table.insert(apps, awful.widget.launcher({image = app.icon, command = app.command }))
    end

    return apps
end

dock.create = create
return dock
