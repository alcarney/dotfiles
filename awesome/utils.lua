-- This is a helper library where I put a load of random functions that 
-- are useful when working with my rc.lua

local naughty = require("naughty")

local function showSignals(obj)

    if not obj._signals then
        naughty.notify({ preset = naughty.config.presets.critical,
                        title = "Signals in obj...",
                        text = "Object given has no signals" })
        return 
    end

    for k,v in pairs(obj._signals) do

        naughty.notify({ preset = naughty.config.presets.normal,
                        title = "Signals in obj...",
                        text = k })
    end
end

local utils = {}

utils.showSignals = showSignals

return utils
