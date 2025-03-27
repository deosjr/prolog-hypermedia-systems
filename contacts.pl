:- dynamic(contacts/5).

all_contacts(All) :-
    findall(c(ID,FirstName,LastName,PhoneNumber,Email), contacts(ID, FirstName, LastName, PhoneNumber, Email), All).

search_contacts(Term, Results) :-
    findall(c(ID,FirstName,LastName,PhoneNumber,Email), (
        contacts(ID, FirstName, LastName, PhoneNumber, Email),
        match(Term, FirstName, LastName, PhoneNumber, Email)
    ), Results).

match(Term, First, _, _, _) :-
    sub_string(First, _, _, _, Term), !.
match(Term, _, Last, _, _) :-
    sub_string(Last, _, _, _, Term), !.
match(Term, _, _, Phone, _) :-
    sub_string(Phone, _, _, _, Term), !.
match(Term, _, _, _, Email) :-
    sub_string(Email, _, _, _, Term), !.

% todo: any form of input validation
save_contact(First, Last, Phone, Email) :-
    findall(ID, contacts(ID,_,_,_,_), IDs),
    max_list(IDs, Max),
    New is Max + 1,
    assertz(contacts(New, First, Last, Phone, Email)).

update_contact(ID, First, Last, Phone, Email) :-
    retract(contacts(ID, _, _, _, _)),
    assertz(contacts(ID, First, Last, Phone, Email)).

delete_contact(ID) :-
    retract(contacts(ID, _, _, _, _)).

contacts(2, "Carson", "Gross", "123-456-7890", "carson@example.comz").
contacts(3, "", "", "", "joe@example2.com").
contacts(5, "Joe", "Blow", "123-456-7890", "joe@example.com").
contacts(6, "Joe", "Blow", "123-456-7890", "joe1@example.com").
contacts(7, "Joe", "Blow", "123-456-7890", "joe2@example.com").
contacts(8, "Joe", "Blow", "123-456-7890", "joe3@example.com").
