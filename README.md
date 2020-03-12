# FsClassRoom

FsClassRoom helps organizing in-class F# coding activities.

### Server

To run the Server, you first install a student CSV file at `db/students.csv`.
The CSV file contains two columns: (1) Full names of students, and (2) Student
IDs.

You also need to provide two FS files to start the server: (1) a library file,
and (2) a test driver file. The library file declares necessary types and
functions in a namespace, and the test driver file contains a series of test
functions, each of which invokes the implementation of `myfunc` to test it.

```
dotnet run --project src/Server -- activities/Lib.fs activities/Tests.fs
```

### Client

To run the Client, you need to write your FS file that contains your own
implementation of the `myfunc` function. This function should be declared in a
module `Namespace.M*`, where `*` is your student ID. This convention is
important, otherwise, your submission will be rejected.

```
dotnet run --project src/Client -- http://SERVER_IP:8080/ <TOKEN> <STUDENT ID> <LAST NAME> <FS FILE>
```
