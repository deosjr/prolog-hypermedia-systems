:- use_module(library(http/http_server)).
:- use_module(library(http/http_client)).
:- use_module(library(dcg/basics)).

:- ['contacts'].

:- initialization
    http_server([port(8080)]).

:- http_handler(root(.),
                http_redirect(moved, location_by_id(contacts)),
                []).
:- http_handler(root(contacts), contacts, []).

http:location(contacts, root(contacts), []).

:- http_handler(contacts(new),
    contacts_new(Method), [method(Method), methods([get,post])]).

:- http_handler(root(contacts/ID),
    contacts_show(ID), [methods([get])]).

:- http_handler(contacts(ID/edit),
    contacts_edit(Method, ID), [method(Method), methods([get,post])]).

:- http_handler(contacts(ID/delete),
    contacts_delete(ID), [methods([post])]).

contacts(Request) :-
    http_parameters(Request, [ q(Search, [default('')]) ]),
    ( Search == '' -> 
        all_contacts(Contacts)
        ;
        search_contacts(Search, Contacts)
    ),
    phrase(layout_head_template, Head),
    index_template(Search, Contacts, Body),
    reply_html_page(Head, Body).

contacts_new(get, _) :-
    phrase(layout_head_template, Head),
    new_template(c(_, '', '', '', ''), Body),
    reply_html_page(Head, Body).

contacts_new(post, Request) :-
    http_parameters(Request, [ first_name(First, []), last_name(Last, []), phone(Phone, []), email(Email, []) ]), 
    ( save_contact(First, Last, Phone, Email) ->
        http_redirect(see_other, location_by_id(contacts), Request)
    ;   phrase(layout_head_template, Head),
        new_template(c(_,First,Last,Phone,Email), Body),
        reply_html_page(Head, Body)).

contacts_show(IDAtom, Request) :- 
    ( atom_number(IDAtom, ID) ->
    ( contacts(ID, First, Last, Phone, Email) ->
        phrase(layout_head_template, Head),
        show_template(c(ID, First, Last, Phone, Email), Body),
        reply_html_page(Head, Body)
    ;   http_404([index(location_by_id(contacts))], Request))
    ;   http_404([index(location_by_id(contacts))], Request)).

contacts_edit(get, IDAtom, Request) :-
    ( atom_number(IDAtom, ID) ->
    ( contacts(ID, First, Last, Phone, Email) ->
        phrase(layout_head_template, Head),
        edit_template(c(ID, First, Last, Phone, Email), Body),
        reply_html_page(Head, Body)
    ;   http_404([index(location_by_id(contacts))], Request))
    ;   http_404([index(location_by_id(contacts))], Request)).

contacts_edit(post, IDAtom, Request) :-
    ( atom_number(IDAtom, ID) ->
    http_parameters(Request, [ first_name(First, []), last_name(Last, []), phone(Phone, []), email(Email, []) ]), 
    ( update_contact(ID, First, Last, Phone, Email) ->
        http_redirect(see_other, location_by_id(contacts(ID)), Request)
    ;   phrase(layout_head_template, Head),
        edit_template(c(ID,First,Last,Phone,Email), Body),
        reply_html_page(Head, Body))
    ;   http_404([index(location_by_id(contacts))], Request)).

contacts_delete(IDAtom, Request) :-
    ( atom_number(IDAtom, ID) ->
        delete_contact(ID),
        http_redirect(see_other, location_by_id(contacts), Request)
    ;   http_404([index(location_by_id(contacts))], Request)).

%% templates

layout_head_template --> [
    title('Contact App'),
    link([rel(stylesheet), href("https://the.missing.style/v0.2.0/missing.min.css")]),
    script([src("https://unpkg.com/htmx.org@1.8.0")], [])
].

layout_body_template(Content) --> 
    [header([h1([div('CONTACTS.APP'), div('A Demo Contacts Application')])])], Content.

% NOTE we do not have access to body div directly, it is added by reply_html_page
layout_template(Content, Body) :-
    phrase(layout_body_template(Content), Out),
    Body = [main('hx-boost'(true), Out)].

index_template(Q, Contacts, Out) :-
    Form = form([action('/contacts'), method(get), class('tool-bar')],[
        label([for(search)], ["Search Term"]),
        input([id(search), type(search), name(q), value(Q)], []),
        input([type(submit), value("Search")], [])
    ]),
    maplist(contact_row, Contacts, Rows),
    Table = table([],[
        thead([
            tr([
                th('First'),
                th('Last'),
                th('Phone'),
                th('Email')
            ])
        ]),
        tbody(Rows)
    ]),
    Content = [
        Form,
        Table,
        p(a(href("/contacts/new"), "Add Contact"))
    ],
    layout_template(Content, Out).

contact_row(c(ID, First, Last, Phone, Email), Row) :-
    create_url(("/contacts/", integer(ID)), ViewLink),
    create_url(("/contacts/", integer(ID), "/edit"), EditLink),
    Edit = td(a(href(EditLink), "Edit")),
    View = td(a(href(ViewLink), "View")),
    Row = tr([ td(First), td(Last), td(Phone), td(Email), Edit, View ]).

new_template(Contact, Out) :-
    Contact = c(_, First, Last, Phone, Email),
    Form = form([action('/contacts/new'), method(post)], [
        fieldset([
            legend("Contact Values"),
            p([
                label([for(email)], ["Email"]),
                input([name(email), id(email), type(email), placeholder("Email"), value(Email)], [])
            ]),
            p([
                label([for(first_name)], ["First Name"]),
                input([name(first_name), id(first_name), type(first_name), placeholder("First Name"), value(First)], [])
            ]),
            p([
                label([for(last_name)], ["Last Name"]),
                input([name(last_name), id(last_name), type(last_name), placeholder("Last Name"), value(Last)], [])
            ]),
            p([
                label([for(phone)], ["Phone"]),
                input([name(phone), id(phone), type(phone), placeholder("Phone"), value(Phone)], [])
            ]),
            button('Save')
        ])
    ]),
    Content = [
        Form,
        p(a(href("/contacts"), "Back"))
    ],
    layout_template(Content, Out).

show_template(Contact, Out) :-
    Contact = c(ID, First, Last, Phone, Email),
    create_url(("/contacts/", integer(ID), "/edit"), EditLink),
    Content = [
        h1([First, Last]),
        div([
            div(["Phone: ", Phone]),
            div(["Email: ", Email])
        ]),
        p([
            a(href(EditLink), "Edit"),
            a(href("contacts"), "Back")
        ])
    ],
    layout_template(Content, Out).

edit_template(Contact, Out) :-
    Contact = c(ID, First, Last, Phone, Email),
    create_url(("/contacts/", integer(ID), "/edit"), EditLink),
    Form = form([action(EditLink), method(post)], [
        fieldset([
            legend("Contact Values"),
            p([
                label([for(email)], ["Email"]),
                input([name(email), id(email), type(email), placeholder("Email"), value(Email)], [])
            ]),
            p([
                label([for(first_name)], ["First Name"]),
                input([name(first_name), id(first_name), type(first_name), placeholder("First Name"), value(First)], [])
            ]),
            p([
                label([for(last_name)], ["Last Name"]),
                input([name(last_name), id(last_name), type(last_name), placeholder("Last Name"), value(Last)], [])
            ]),
            p([
                label([for(phone)], ["Phone"]),
                input([name(phone), id(phone), type(phone), placeholder("Phone"), value(Phone)], [])
            ]),
            button('Save')
        ])
    ]),
    create_url(("/contacts/", integer(ID), "/delete"), DeleteLink),
    Content = [
        Form,
        form([action(DeleteLink), method(post)], [button('Delete Contact')]),
        p(a(href("/contacts"), "Back"))
    ],
    layout_template(Content, Out).

create_url(PhraseBody, URL) :-
    phrase(PhraseBody, Codes),
    string_codes(URL, Codes).
