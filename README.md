Library-server
==============

Introduction
====

Here's a basic application to practic Generic Server and  Mnesia together,
this example is about a library, where  you can storage information about
the rents from the books.

* Generic Sever 

A behaviour module for implementing the server of a client-server relation. 
A generic server process (gen_server) implemented using this module will 
have a standard set of interface functions and include functionality for 
tracing and error reporting. It will also fit into an OTP supervision tree. 

more info: http://www.erlang.org/doc/man/gen_server.html

* Mnesia

Mnesia is a distributed DataBase Management System (DBMS), appropriate for 
telecommunications applications and other Erlang applications which require 
continuous operation and exhibit soft real-time properties.

more info: http://www.erlang.org/doc/man/mnesia.html

How to start
====

clone the project:

  	$ git clone git://github.com/nezt/library-server.git

Into library:

	$ cd library-server

Let's compile and start the application:

	$ make compile && make start

Ready, now test the example:


First let's create books in the library with a Key and name of the book:

	> library_server:insert_book(Id,NameBook).
  

You can see the books list, according to their status:
                                               
	> library_server:get_books_all().
	> library_server:get_books_available().
	> library_server:get_books_unavailable().

to rent a book:

	> library_server:rent(Who, NameBook).

Note: Who is a name of a person.

to return a book:
	
	> library_server:return_book(NameBook). 
			