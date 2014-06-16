(setq ediff-split-window-function (if (> (frame-width) 150 )
                                      'split-window-horizontally
                                    'split-window-vertically))
