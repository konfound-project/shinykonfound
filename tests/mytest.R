app <- ShinyDriver$new("../")
app$snapshotInit("mytest")

app$setInputs(button = "click")
app$snapshot()
app$snapshot()
app$snapshot()
app$setInputs(button_nl = "click")
app$snapshot()
