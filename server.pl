:- use_module(library(http/http_server)).
:- use_module(library(dcg/basics)).

:- ['contacts'].

:- initialization
    http_server([port(8080)]).

:- http_handler(root(.),
                http_redirect(moved, location_by_id(contacts)),
                []).
:- http_handler(root(contacts), contacts, []).

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

%% templates

layout_head_template --> [
    title('Contact App'),
    link([rel(stylesheet), href("https://the.missing.style/v0.2.0/missing.min.css")]),
    script([src("https://unpkg.com/htmx.org@1.8.0")], [])
].

layout_body_template(Content) --> 
    [header([h1([div('CONTACTS.APP'), div('A Demo Contacts Application')])])], Content.

index_template(Q, Contacts, [main(Out)]) :-
    Form = form([action('/contacts'), method(get), class('tool-bar')],[
        label([for(search)], ["Search Term"]),
        input([id(search), type(search), name(q), value(Q)], []),
        input([type(submit), value("Search")], [])
    ]),
    maplist(contactrow, Contacts, Rows),
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
        Table
    ],
    phrase(layout_body_template(Content), Out).

contactrow(c(ID, First, Last, Phone, Email), Row) :-
    phrase(("/contacts/", integer(ID)), ViewCodes),
    string_codes(ViewLink, ViewCodes),
    phrase(("/contacts/", integer(ID), "/edit"), EditCodes),
    string_codes(EditLink, EditCodes),
    Edit = td(a(href(EditLink), "Edit")),
    View = td(a(href(ViewLink), "View")),
    Row = tr([ td(First), td(Last), td(Phone), td(Email), Edit, View ]).
