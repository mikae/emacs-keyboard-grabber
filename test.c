#include <xcb/xcb.h>
#include <xcb/xproto.h>
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>

const char* get_display_name(const char *display_name)
{
    const char *name = display_name;

    if (!name) {
        name = getenv ("DISPLAY");
        if (!name)
            name = "";
    }
    return (name);
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

struct atom_cache_entry {
    xcb_atom_t atom;
    const char *name;
    xcb_intern_atom_cookie_t intern_atom;
    struct atom_cache_entry *next;
};

static struct atom_cache_entry *atom_cache;

struct atom_cache_entry *intern_atom (xcb_connection_t *dpy, const char *name)
{
    struct atom_cache_entry *a;

    for (a = atom_cache ; a != NULL ; a = a->next) {
        if (strcmp (a->name, name) == 0)
            return a;
    }

    a = calloc(1, sizeof(struct atom_cache_entry));
    if (a != NULL) {
        a->name = name;
        a->intern_atom = xcb_intern_atom (dpy, 0, strlen (name), (name));
        a->next = atom_cache;
        atom_cache = a;
    }
    return a;
}

xcb_atom_t get_atom(xcb_connection_t *dpy, const char *name)
{
    struct atom_cache_entry *a = intern_atom(dpy, name);

    if (a == NULL)
        return XCB_ATOM_NONE;

    if (a->atom == XCB_ATOM_NONE) {
        xcb_intern_atom_reply_t *reply;

        reply = xcb_intern_atom_reply(dpy, a->intern_atom, NULL);
        if (reply) {
            a->atom = reply->atom;
            free (reply);
        } else {
            a->atom = -1;
        }
    }
    if (a->atom == -1)
        return XCB_ATOM_NONE;

    return a->atom;
}
void setup_display_and_screen(
                              const char *display_name,
                              xcb_connection_t **dpy,	/* MODIFIED */
                              xcb_screen_t **screen)	/* MODIFIED */
{
    int screen_number, i, err;

    *dpy = xcb_connect (display_name, &screen_number);
    if ((err = xcb_connection_has_error (*dpy)) != 0) {
        switch (err) {
        case XCB_CONN_CLOSED_MEM_INSUFFICIENT:
            fatal_error("Failed to allocate memory in xcb_connect");
        case XCB_CONN_CLOSED_PARSE_ERR:
            fatal_error("unable to parse display name \"%s\"",
                        get_display_name(display_name) );
        default:
            fatal_error("unable to open display \"%s\"",
                        get_display_name(display_name) );
        }
    }

    if (screen) {
        /* find our screen */
        const xcb_setup_t *setup = xcb_get_setup(*dpy);
        xcb_screen_iterator_t screen_iter = xcb_setup_roots_iterator(setup);
        int screen_count = xcb_setup_roots_length(setup);
        if (screen_count <= screen_number)
            {
                fatal_error("unable to access screen %d, max is %d",
                            screen_number, screen_count-1 );
            }

        for (i = 0; i < screen_number; i++)
            xcb_screen_next(&screen_iter);
        *screen = screen_iter.data;
    }
}

void* get_window_property(xcb_connection_t *dpy,
                          xcb_window_t window,
                          xcb_atom_t property,
                          xcb_atom_t type,
                          uint32_t length) {
    xcb_get_property_cookie_t cookie;
    xcb_get_property_reply_t *reply;
    void* value;
    int actual_length;

    value = NULL;
    cookie = xcb_get_property(dpy,
                              0,
                              window,
                              property,
                              type,
                              0,
                              length);

    if ((reply = xcb_get_property_reply(dpy, cookie, NULL))) {
        actual_length = xcb_get_property_value_length(reply);

        if (actual_length > 0) {
            value = calloc(actual_length, sizeof(uint32_t));
            memcpy(value, xcb_get_property_value(reply), actual_length * sizeof(uint32_t));
        }

        free(reply);
    }

    return value;
}

pid_t get_window_pid(xcb_connection_t *dpy,
                     xcb_window_t window)
{
    pid_t pid = -1;
    void* value = get_window_property(dpy,
                                      window,
                                      get_atom(dpy, "_NET_WM_PID"),
                                      XCB_ATOM_CARDINAL,
                                      1);
    if (value) {
        pid = *((pid_t*)value);
        free(value);
    }
    return pid;
}

char* get_window_name(xcb_connection_t *dpy,
                      xcb_window_t window)
{
    char* name = NULL;
    void* value = get_window_property(dpy,
                                      window,
                                      XCB_ATOM_WM_NAME,
                                      XCB_ATOM_STRING,
                                      10);
    if (value) {
        printf("%s\n", (char*)value);
        name = (char*) value;
    }

    return name;
}

xcb_window_t* find_window_by_pid(xcb_connection_t *dpy, xcb_window_t window, const pid_t target_pid) {
    xcb_query_tree_cookie_t cookie;
    xcb_query_tree_reply_t* reply;
    xcb_window_t* found;
    pid_t pid;
    int i;

    found = NULL;

    cookie = xcb_query_tree(dpy, window);
    if ((reply = xcb_query_tree_reply(dpy, cookie, NULL))) {
        xcb_window_t *children = xcb_query_tree_children(reply);
        int length = xcb_query_tree_children_length(reply);
        for(i = 0; i < length && !found; i++) {
            xcb_window_t child = children[i];
            pid = get_window_pid(dpy, child);

            if (!found && pid == target_pid) {
                found = calloc(1, sizeof(xcb_window_t));
                memcpy(found, &child, sizeof(xcb_window_t));
            }

            if (!found) {
                found = find_window_by_pid(dpy, child, target_pid);
            }
        }
            free(reply);
        }

        return found;
    }

void print_info(xcb_connection_t *dpy, xcb_window_t window) {
    char* name = get_window_name(dpy, window);
    printf("Name: %s\n", name);
    free(name);
}

int parse_pid(int argc, char **argv) {
    if (argc != 2) {
        fatal_error("First arg must be valid pid");
    }

    return (int)strtol(argv[1], NULL, 10);
}

void grab_keyboard(xcb_connection_t *dpy, xcb_window_t window) {
    xcb_grab_key(dpy,
                 0,
                 window,
                 XCB_MOD_MASK_ANY,
                 XCB_GRAB_ANY,
                 XCB_GRAB_MODE_ASYNC, XCB_GRAB_MODE_ASYNC);
    xcb_flush(dpy);

    /* xcb_ungrab_key(dpy, XCB_GRAB_ANY, window, XCB_MOD_MASK_ANY); */
    /* xcb_flush(dpy); */

    xcb_generic_event_t *ev;
    xcb_key_press_event_t *kev;
    while ((ev = xcb_wait_for_event(dpy))) {
        switch (ev->response_type & ~0x80) {
        case XCB_KEY_PRESS:
            kev = (xcb_key_press_event_t*)ev;
            printf("Pressed: %d\n", kev->detail);
            break;
        case XCB_KEY_RELEASE:
            kev = (xcb_key_press_event_t*)ev;
            printf("Released: %d\n", kev->detail);
            break;
        }

        free(ev);
    }
}

int main(int argc, char **argv) {
    const pid_t pid = parse_pid(argc, argv);
    xcb_connection_t *dpy;
    xcb_screen_t *screen;
    xcb_window_t *found;
    xcb_window_t window;

    setup_display_and_screen(NULL, &dpy, &screen);
    window = screen->root;
    found = find_window_by_pid(dpy, window, pid);

    if (found) {
        printf("Found :3\n");
        printf("window: 0x%08x\n", *found);
        print_info(dpy, window);
        grab_keyboard(dpy, *found);
    }

    xcb_disconnect(dpy);
    return 0;
}
