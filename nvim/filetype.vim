" Settings for the markdown folding plugin
if exists ("did_load_filetypes")
    finish
endif
autocmd FileType markdown set foldexpr=NestedMardownFolds()
