function data()
    return {
        info = {
            minorVersion = 4,
            severityAdd = "NONE",
            severityRemove = "CRITICAL",
            name = _("name"),
            description = _("desc"),
            authors = {
                {
                    name = "Enzojz",
                    role = "CREATOR",
                    text = "Idea, Scripting, Modeling",
                    steamProfile = "enzojz",
                    tfnetId = 27218,
                }
            },
            tags = {"Train Station", "Station"},
        },
        runFn = function(_)
            game.config.undergroundStationMod = true
        end
    }
end
