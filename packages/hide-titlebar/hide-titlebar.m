#import <Cocoa/Cocoa.h>
#include "emacs-module.h"

int plugin_is_GPL_compatible;

static emacs_value
Fhide_titlebar(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data)
{
    dispatch_async(dispatch_get_main_queue(), ^{
        NSApplication *app = [NSApplication sharedApplication];
        for (NSWindow *window in [app windows]) {
            [window setTitleVisibility:NSWindowTitleHidden];
            [window setTitlebarAppearsTransparent:YES];

            [[window standardWindowButton:NSWindowCloseButton] setHidden:YES];
            [[window standardWindowButton:NSWindowMiniaturizeButton] setHidden:YES];
            [[window standardWindowButton:NSWindowZoomButton] setHidden:YES];

            // Make the content view extend into the title bar area
            window.styleMask |= NSWindowStyleMaskFullSizeContentView;
        }
    });

    return env->intern(env, "t");
}

int
emacs_module_init(struct emacs_runtime *runtime)
{
    emacs_env *env = runtime->get_environment(runtime);

    emacs_value func = env->make_function(env, 0, 0, Fhide_titlebar,
        "Hide title bar buttons and make titlebar transparent on all frames.",
        NULL);

    emacs_value symbol = env->intern(env, "hide-titlebar--do-hide");
    emacs_value args[] = { symbol, func };
    env->funcall(env, env->intern(env, "defalias"), 2, args);

    emacs_value feature = env->intern(env, "hide-titlebar");
    emacs_value provide_args[] = { feature };
    env->funcall(env, env->intern(env, "provide"), 1, provide_args);

    return 0;
}
