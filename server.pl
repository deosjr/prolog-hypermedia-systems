:- use_module(library(http/http_server)).

:- initialization
    http_server([port(8080)]).

:- http_handler(root(.),
                http_redirect(moved, location_by_id(contacts)),
                []).
:- http_handler(root(contacts), contacts, []).

contacts(Request) :-
    http_parameters(Request, [ q(Search, [optional(true)]) ]),
    ( var(Search) -> 
        Contacts='todo findall'
        ;
        Contacts='todo search'
    ),
    layout_head_template(Head),
    index_template(Contacts, Body),
    reply_html_page(Head, Body).

%% templates

layout_head_template([
    title('Contact App'),
    link([rel(stylesheet), href("https://the.missing.style/v0.2.0/missing.min.css")]),
    script([src("https://unpkg.com/htmx.org@1.8.0")], [])
]).

layout_template(Content) --> 
    [header([h1([div('CONTACTS.APP'), div('A Demo Contacts Application')])])], Content.

index_template(Contacts, [main(Out)]) :-
    Content = [
        h2('Contacts'),
        div(Contacts)
    ],
    phrase(layout_template(Content), Out).
