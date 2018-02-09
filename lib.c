#include <xcb/xcb.h>
#include <xcb/xproto.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <unistd.h>

#include "lib.h"

// -----------------------------------------------------------------------------
static struct atom_cache_entry *atom_cache;
static struct xcb_connection_t *xcb_connection;
static xcb_screen_t *xcb_screen;
static int screen_number;

// -----------------------------------------------------------------------------
struct atom_cache_entry *
get_or_create_atom_cache_entry(const char *name);

struct atom_cache_entry *
get_atom_cache_entry(const char *name);

int check_is_null(void *a);
int check_atom_is_none(struct atom_cache_entry *a);

struct atom_cache_entry *
create_new_atom_cache_entry(const char *name);

void save_atom_cache_entry(struct atom_cache_entry *a);

void
update_atom_in_cache(struct atom_cache_entry *a);

xcb_atom_t
get_atom_by_cookie(xcb_intern_atom_cookie_t cookie);

xcb_get_property_cookie_t
get_property_cookie(struct window_property_selector selector);

void*
get_property_value_by_cookie(xcb_get_property_cookie_t cookie);

void*
clone_property_value(xcb_get_property_reply_t *reply);

pid_t
extract_pid(void* value);

xcb_window_t*
find_window_by_pid(xcb_window_t window,
                   const pid_t target_pid);

void
setup_xcb_connection(const char *display_name);

void
setup_screen();

// -----------------------------------------------------------------------------
xcb_atom_t
get_atom_by_name(const char *name)
{
    struct atom_cache_entry *a = get_or_create_atom_cache_entry(name);

    update_atom_in_cache(a);

    return a->atom;
}

struct atom_cache_entry *
get_or_create_atom_cache_entry(const char *name)
{
    struct atom_cache_entry *a;

    a = get_atom_cache_entry(name);

    if (check_is_null(a)) {
        a = create_new_atom_cache_entry(name);
        save_atom_cache_entry(a);
    }

    return a;
}

struct atom_cache_entry *
get_atom_cache_entry(const char *name)
{
    struct atom_cache_entry *a = NULL;

    for (a = atom_cache ; a != NULL ; a = a->next_entry) {
        if (strcmp (a->name, name) == 0)
            break;
    }

    return a;
}

int
check_is_null(void *a) {
    return a == NULL;
}

struct atom_cache_entry *
create_new_atom_cache_entry(const char *name) {
    struct atom_cache_entry *a = NULL;

    a = calloc(1, sizeof(struct atom_cache_entry));

    if (a != NULL) {
        a->name = name;
        a->cookie = xcb_intern_atom(xcb_connection,
                                    0,
                                    strlen(name),
                                    (name));
        a->atom = XCB_ATOM_NONE;
    }

    return a;
}

void
save_atom_cache_entry(struct atom_cache_entry *a) {
    a->next_entry = atom_cache;
    atom_cache = a;
}

void
update_atom_in_cache(struct atom_cache_entry *a) {
    if ((!check_is_null(a)) && check_atom_is_none(a)) {
        a->atom = get_atom_by_cookie(a->cookie);
    }
}

int
check_atom_is_none(struct atom_cache_entry *a) {
    return a->atom == XCB_ATOM_NONE;
}

xcb_atom_t
get_atom_by_cookie(xcb_intern_atom_cookie_t cookie) {
    xcb_intern_atom_reply_t *reply;
    xcb_atom_t atom = XCB_ATOM_NONE;

    reply = xcb_intern_atom_reply(xcb_connection, cookie, NULL);
    if (reply) {
        atom = reply->atom;
        free(reply);
    }

    return atom;
}

// here
void*
get_window_property(struct window_property_selector selector)
{
    xcb_get_property_cookie_t cookie = get_property_cookie(selector);
    return get_property_value_by_cookie(cookie);
}

xcb_get_property_cookie_t
get_property_cookie(struct window_property_selector selector) {
    return xcb_get_property(xcb_connection,
                            0,
                            selector.window,
                            selector.property,
                            selector.type,
                            0,
                            selector.length);
}

void*
get_property_value_by_cookie(xcb_get_property_cookie_t cookie) {
    xcb_get_property_reply_t *reply;
    void* value = NULL;

    reply = xcb_get_property_reply(xcb_connection, cookie, NULL);

    if (! check_is_null(reply)) {
        value = clone_property_value(reply);
        free(reply);
    }

    return value;
}

void*
clone_property_value(xcb_get_property_reply_t *reply) {
    void* value = NULL;
    int actual_length = xcb_get_property_value_length(reply);

    if (actual_length > 0) {
        value = calloc(actual_length, sizeof(uint32_t));
        memcpy(value, xcb_get_property_value(reply), actual_length * sizeof(uint32_t));
    }

    return value;
}

pid_t
get_window_pid(xcb_window_t window)
{
    pid_t pid = -1;
    struct window_property_selector selector =
        {
            .window = window,
            .property = get_atom_by_name("_NET_WM_PID"),
            .type = XCB_ATOM_CARDINAL,
            .length = 1
        };

    void* value = get_window_property(selector);
    if (! check_is_null(value)) {
        pid = extract_pid(value);
        free(value);
    }

    return pid;
}

pid_t
extract_pid(void* value) {
    return *((pid_t*)value);
}

// here
void
setup_display_and_screen(const char *display_name)
{
    setup_xcb_connection(display_name);
    setup_screen();
}

void
setup_xcb_connection(const char *display_name) {
    xcb_connection = xcb_connect(display_name, &screen_number);
    int err = xcb_connection_has_error (xcb_connection);

    if (err != 0) {
        close_xcb_connection();
    }
}

void
setup_screen() {
    int i;

    const xcb_setup_t *setup = xcb_get_setup(xcb_connection);
    xcb_screen_iterator_t screen_iter = xcb_setup_roots_iterator(setup);
    int screen_count = xcb_setup_roots_length(setup);

    if (screen_count <= screen_number) {
        close_xcb_connection();
    } else {
        for (i = 0; i < screen_number; i++) {
            xcb_screen_next(&screen_iter);
        }

        xcb_screen = screen_iter.data;
    }
}


xcb_window_t*
find_window_by_pid_from_root(const pid_t target_pid) {
    return find_window_by_pid(xcb_screen->root, target_pid);
}

xcb_window_t*
find_window_by_pid(xcb_window_t window,
                   const pid_t target_pid)
{
    xcb_query_tree_cookie_t cookie;
    xcb_query_tree_reply_t* reply;
    xcb_window_t* found;
    pid_t pid;
    int i;

    found = NULL;

    cookie = xcb_query_tree(xcb_connection, window);
    if ((reply = xcb_query_tree_reply(xcb_connection, cookie, NULL))) {
        xcb_window_t *children = xcb_query_tree_children(reply);
        int length = xcb_query_tree_children_length(reply);

        for(i = 0; i < length && !found; i++) {
            xcb_window_t child = children[i];
            pid = get_window_pid(child);

            if (!found && pid == target_pid) {
                found = calloc(1, sizeof(xcb_window_t));
                memcpy(found, &child, sizeof(xcb_window_t));
            }

            if (!found) {
                found = find_window_by_pid(child, target_pid);
            }
        }
        free(reply);
    }

    return found;
}

void
grab_keyboard(xcb_window_t window) {
    xcb_grab_key(xcb_connection,
                 0,
                 window,
                 XCB_MOD_MASK_ANY,
                 XCB_GRAB_ANY,
                 XCB_GRAB_MODE_ASYNC, XCB_GRAB_MODE_ASYNC);
    xcb_flush(xcb_connection);
}

void
ungrab_keyboard(xcb_window_t window) {
    xcb_ungrab_key(xcb_connection,
                   XCB_GRAB_ANY,
                   window,
                   XCB_MOD_MASK_ANY);
    xcb_flush(xcb_connection);
}

void
close_xcb_connection() {
    if (! check_is_null(xcb_connection)) {
        xcb_disconnect(xcb_connection);
        xcb_connection = NULL;
        xcb_screen = NULL;
    }
}

xcb_generic_event_t *
read_xcb_event() {
    return xcb_poll_for_event(xcb_connection);
}

xcb_generic_event_t *
read_xcb_event_sync() {
    return xcb_wait_for_event(xcb_connection);
}
