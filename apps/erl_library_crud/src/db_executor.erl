-module(db_executor).

-export([db_receiver/0]).
-export([get_books/1]).

db_receiver() ->
    {ok, C} = epgsql:connect(#{
        host => "localhost",
        username => "postgres",
        password => "postgres",
        database => "library",
        timeout => 4000
    }),
    db_receive(C).


db_receive(C) ->
    receive
        {get, BookId, Pid} ->
            Pid ! get_book(C, BookId);
        {get_all, Pid} ->
            Pid ! get_books(C);
        {create, Book, Pid} ->
            Pid ! create_book(C, Book);
        {update, BookId, Book, Pid} ->
            Pid ! update_book(C, BookId, Book);
        {delete, BookId, Pid} ->
            Pid ! delete_book(C, BookId)
    end,
    db_receive(C).


get_book(C, BookId) ->
    {ok, _, Rows} = epgsql:equery(C, "SELECT * FROM books WHERE id = $1", [BookId]),
    Rows.


get_books(C) ->
    {ok, _, Rows} = epgsql:squery(C, "SELECT * FROM books"),
    Rows.


create_book(C, #{<<"Title">> := Title, <<"Author">> := Author}) ->
    {ok, _, _, Rows} = epgsql:equery(C, "INSERT INTO books (title, author) VALUES ($1, $2) RETURNING *", [Title, Author]),
    Rows.


update_book(C, BookId, #{<<"Title">> := Title, <<"Author">> := Author}) ->
    {ok, _, _, Rows} = epgsql:equery(C, "UPDATE books SET title = $1, author = $2 WHERE id = $3 RETURNING *", [Title, Author, BookId]),
    Rows.


delete_book(C, BookId) ->
    {ok, Count} = epgsql:equery(C, "DELETE FROM books WHERE id = $1", [BookId]),
    Count.
