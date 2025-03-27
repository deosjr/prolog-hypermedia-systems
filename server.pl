:- use_module(library(http/http_server)).

:- ['contacts'].

:- initialization
    http_server([port(8080)]).

:- http_handler(root(.),
                http_redirect(moved, location_by_id(contacts)),
                []).
:- http_handler(root(contacts), contacts, []).

contacts(Request) :-
    http_parameters(Request, [ q(Search, [optional(true)]) ]),
    ( var(Search) -> 
        all_contacts(Contacts)
        ;
        search_contacts(Search, Contacts)
    ),
    phrase(layout_head_template, Head),
    index_template(Contacts, Body),
    reply_html_page(Head, Body).

%% templates

layout_head_template --> [
    title('Contact App'),
    link([rel(stylesheet), href("https://the.missing.style/v0.2.0/missing.min.css")]),
    script([src("https://unpkg.com/htmx.org@1.8.0")], [])
].

layout_body_template(Content) --> 
    [header([h1([div('CONTACTS.APP'), div('A Demo Contacts Application')])])], Content.

index_template(Contacts, [main(Out)]) :-
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
        Table
    ],
    phrase(layout_body_template(Content), Out).

contactrow(c(_, First, Last, Phone, Email), Row) :-
    Row = tr([ td(First), td(Last), td(Phone), td(Email) ]).
