# Get rid of the alt-menu shortcuts
atom.menu.template.forEach (t) ->
  t.label = t.label.replace("&", "")
atom.menu.update()