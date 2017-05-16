# FunFS Demo Driver

## Introduction

This file introduces the `FunFS` filesystem library, which implements a demo Ext2 filesystem driver in Haskell.

A PDF version of this file can be found in the `dist/doc` subfolder.

---

## Description

`FunFS` (Functional FileSystem) is a functional implementation of the Second Extended Filesystem (Ext2) in Haskell. It features a lazy-evaluated, monadic UNIX-like API which hides the details of the underlying filesystem, allowing replacements to be dropped in at a later time without modifying client code.

---

## Building

### Build system

The project is built using `cabal`, the Haskell build system which is distributed with the Haskell Platform.

The Haskell Platform can be downloaded from <https://www.haskell.org/platform/>.

### Dependencies

`FunFS` depends on the following packages:

| Package name       | Minimum    | Maximum   |
| -------------------|-----------:|-----------|
| `ghc`              | `7.2.1`    |           |
| `base`             | `4.9`      | `4.10`    |
| `binary`           | `0.8.3`    | `0.9`     |
| `bytestring`       | `0.10.8.1` | `0.11`    |
| `mtl`              | `2.2`      | `2.3`     |
| `QuickCheck`       | `2.9.2`    | `2.10`    |
| `template-haskell` | `2.11`     | `2.12`    |
| `time`             | `1.6.0.1`  | `1.7`     |
| `transformers`     | `0.5.2`    | `0.6`     |
| `uuid`             | `1.2.2`    | `1.3`     |

Most of these are distributed with the Haskell Platform. The only ones which should need to be installed manually are:
 * `quickcheck`
 * `uuid`

Due to the dependencies on `ghc` and `template-haskell`, which is a `GHC` extension, other Haskell compilers like `hugs` will probably not work.

Once Haskell is installed, any packages can be installed with `cabal`, the Haskell package manager:

```sh
$ cabal update
$ cabal install <package name>
```

### Library and examples

Finally, the library and examples can be built with `cabal`:

```sh
$ cabal configure
$ cabal build
```

This builds the library and examples.

### Running the examples

Before executing the examples we should create an Ext2 filesystem:
```sh
$ dd if=/dev/zero of="$device" bs=1M count=64
$ mke2fs -r0 "$device"
```

This creates a 64 MB filesystem and formats it as Ext2 revision 0. The file can now be used to run the examples:

 * `Example0_Mount`

    The `mount` example mounts a filesystem and prints the contents of its superblock and block group descriptor table.

   ```sh
   $ dist/build/mount/mount "$device"
   ```

    Its output can be compared to the output of the `dumpe2fs` command on Linux.

 * `Example1_ReadDir`

   The `readdir` example mounts a filesystem, searches for a path, and lists the contents of the directory named by the path.

   ```sh
   $ dist/build/readdir/readdir "$device" "$path"
   ```

   Its output can be compared to `ls -al`.

   *Note: To run the real `ls` on <device>, we will need to use UNIX loop devices. This will be explained in the next section.*

 * `Example2_ReadFile`

   The `readfile` example mounts a filesystem, searches for a path, and dumps the contents of the file named by the path.

   ```sh
   $ dist/build/readfile/readfile "$device" "$path"
   ```

   Its output can be compared with `cat`.

   However, it's important to point out that a fresh Ext2 filesystem does not contain any files, just two directories: `/` and `/lost+found`.

   To test this example we will need to create a file on the Ext2 filesystem. The following commands create a 'loop-back device' using `losetup` and mount a filesystem using `mount`. (This requires superuser access.)

   ```sh
   $ loop=$(losetup -f "$device")
   $ mount -o ro "$loop" "$mountpoint"
   ```

   **NB**: The `-o ro` flags to `mount` are very important. Without this, the Ext2 driver will update the superblock to revision 1, which is not supported by `FunFS`.

   A 50 MiB file containing random data is created on the filesystem, which is then unmounted. We also keep a local copy of the file for comparison.

   ```sh
   $ dd if=/dev/urandom of="$mountpoint/foo" bs=1M count=50
   $ cp "$mountpoint/foo" "bar"
   $ umount "$loop"
   $ losetup -d "$loop"
   ```

  Finally, `readfile` is executed to get the contents of the file and its output is compared to the original file using `diff`.

  ```sh
  $ dist/build/readfile/readfile "$device" "/foo" >foo
  $ diff foo bar
  ```

  The return value of `diff` (`$?`) will be 0 if the files are the same. As `foo` and `bar` are binary files, if their contents are different, `diff` will say `"Binary files differ"`.

---

## API Documentation
Documentation for the public interfaces of every module is in the `doc/html` directory. This is automatically-generated HTML documentation pulled directly from source code but is quite detailed and neatly formatted. This can be viewed in the browser at `doc/html/index.html`.

---
