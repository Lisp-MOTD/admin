## Lisp Message of the Day (Common Lisp Admin Utilities)

### Overview

This package contains tools used by admins to maintain the Common Lisp
Message of the Day service.

### Creating A Public Key and Private Key Pair

To authenticate as an admin, you need to have a DSA private key and an
associated user name and a password to use to encrypt the private key
on disk.

    (motd-admin:generate-key-pair user-name password)

This will create two files.  It will create a file named `user-name`
with the `.key` extension which contains the private key encrypted
with the given `password`.  It will create a file name `user-name`
with the `.pub` extension which contains the public key.

Before the key pair can be used, the public key needs to be added to
the database by someone who already has admin permissions.

### Login/Logout

Once you have a private key, you can login using the `login` function
and providing the user name and password you used when generating the
key pair:

    (motd-admin:login user-name password)

Once logged in, you can logout at any time using the `logout`
function:

    (motd-admin:logout)

### License

This code is available through the [UNLICENSE][UN].

[UN]: http://unlicense.org/
