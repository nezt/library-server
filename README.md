library-server
==============

This is a basic application to practic Generic Server and  Mnesia together.


* Introduction

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

* How to start

clone the project:

  $ git clone https://github.com/nezt/hl72xml.git

Into library:

	$ cd library

Let's compile and start the application:

	$ make compile && make start

Ready, now test the example:


First let's to create books in the library with a Key and name of the book:

	> library_server:insert_book(Id,NameBook).
  

You can see the books list, according to their status:
                                               
	> get_books_all().
	> get_books_available().
        > get_books_unable().

to rent a book:

	> rent(Who, NameBook).

tu return a book:
	
        > return_book(NameBook). 
			