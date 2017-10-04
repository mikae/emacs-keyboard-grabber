#include <xcb/xcb.h>
#include <xcb/xproto.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <unistd.h>

#include "lib.h"

static struct atom_cache_entry *atom_cache;

struct atom_cache_entry *
intern_atom (xcb_connection_t *dpy,
             const char *name)
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

xcb_atom_t
get_atom(xcb_connection_t *dpy,
         const char *name)
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

void*
get_window_property(xcb_connection_t *dpy,
                    xcb_window_t window,
                    xcb_atom_t property,
                    xcb_atom_t type,
                    uint32_t length)
{
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

pid_t
get_window_pid(xcb_connection_t *dpy,
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

void
setup_display_and_screen(
                         const char *display_name,
                         xcb_connection_t **dpy,
                         xcb_screen_t **screen)
{
    int screen_number, i, err;

    *dpy = xcb_connect (display_name, &screen_number);
    if ((err = xcb_connection_has_error (*dpy)) != 0) {
        if (*dpy) {
            xcb_disconnect(*dpy);
        }
        *dpy = NULL;
        return;
    }

    if (screen) {
        /* find our screen */
        const xcb_setup_t *setup = xcb_get_setup(*dpy);
        xcb_screen_iterator_t screen_iter = xcb_setup_roots_iterator(setup);
        int screen_count = xcb_setup_roots_length(setup);

        if (screen_count <= screen_number) {
            if (*dpy) {
                xcb_disconnect(*dpy);
            }
            *dpy = NULL;
            *screen = NULL;
            return;
        }

        for (i = 0; i < screen_number; i++) {
            xcb_screen_next(&screen_iter);
        }
        *screen = screen_iter.data;
    }
}

xcb_window_t*
find_window_by_pid(xcb_connection_t *dpy,
                   xcb_window_t window,
                   const pid_t target_pid)
{
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
