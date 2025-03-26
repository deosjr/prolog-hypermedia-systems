:- use_module(library(http/http_server)).

:- initialization
    http_server([port(8080)]).

:- http_handler(root(.),
                http_redirect(moved, location_by_id(contacts)),
                []).
:- http_handler(root(contacts), contacts, []).

contacts(_Request) :-
    reply_html_page(
        title('Hypermedia systems'),
        [ h1('Hello world!')
        ]).
