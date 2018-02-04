#include <xcb/xcb.h>
#include <xcb/xproto.h>
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "lib.h"

char* get_window_name(xcb_window_t window)
{
    char* name = NULL;
    struct window_property_selector selector =
        {
            .window = window,
            .property = XCB_ATOM_WM_NAME,
            .type = XCB_ATOM_STRING,
            .length = 10
        };
    void* value = get_window_property(selector);

    if (value) {
        printf("%s\n", (char*)value);
        name = (char*) value;
    }

    return name;
}

void print_info(xcb_window_t window) {
    char* name = get_window_name(window);
    printf("Name: %s\n", name);
    free(name);
}


void fatal_error (const char *msg, ...)
{
    va_list args;
    fflush (stdout);
    fflush (stderr);
    fprintf (stderr, "%s: error: ", "test");
    va_start (args, msg);
    vfprintf (stderr, msg, args);
    va_end (args);
    fprintf (stderr, "\n");
    exit (EXIT_FAILURE);
}

int parse_pid(int argc, char **argv) {
    if (argc != 2) {
        fatal_error("First arg must be valid pid");
    }

    return (int)strtol(argv[1], NULL, 10);
}

int main(int argc, char** argv) {
    const pid_t pid = parse_pid(argc, argv);
    xcb_window_t *found;

    setup_display_and_screen(NULL);
    found = find_window_by_pid_from_root(pid);

    if (found) {
        printf("Found :3\n");
        printf("window: 0x%08x\n", *found);
        /* print_info(dpy, window); */
        grab_keyboard(*found);
    }

    xcb_generic_event_t *ev;
    xcb_key_press_event_t *kev;

    int i;
    for(i = 0; i < 20; i++) {
        ev = read_xcb_event();
        if (ev) {
            switch (ev->response_type & ~0x80) {
            case XCB_KEY_PRESS:
                kev = (xcb_key_press_event_t*) ev;
                printf("Pressed: %d\n", kev->detail);
                break;

            case XCB_KEY_RELEASE:
                kev = (xcb_key_press_event_t*) ev;
                printf("Released: %d\n", kev->detail);
                break;
            }
            free(ev);
        }
        sleep(1);
    }

    close_xcb_connection();
    return 0;
}
