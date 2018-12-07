# Commit log
---
###### commit  `85a5b9fe2eb551c44f4194b501b345374d9e1e41`
###### Author: `Chris Swinchatt` <chrisswinchatt@gmail.com>
###### Date: `Wed May 10 10:35:09 2017-2018 +0100`

    Add example Example2_ReadFile
---
###### commit `59a524da6299a73482ab0600ddca96d1d8280a44`
###### Author: `Chris Swinchatt` <chrisswinchatt@gmail.com>
###### Date: `  Wed May 10 10:35:09 2017-2018 +0100`

    Overhaul of API

    Summary of changes:
     * Implement file I/O
     * Fix directory API
     * General code cleanup
---
###### commit `62a44db8b8408ee65ad24521d17a12440e4be841`
###### Author: `Chris Swinchatt` <chrisswinchatt@gmail.com>
###### Date: `  Wed May 10 10:35:09 2017-2018 +0100`

    Add example Example1_ReadDir which lists the contents of a directory

    Also fix bugs in directory system
---
###### commit `88aa712bf2e5878725356d9b6c6d8246c2d069d7`
###### Author: `Chris Swinchatt` <chrisswinchatt@gmail.com>
###### Date: `  Wed May 10 10:35:08 2017-2018 +0100`

    Add module header to Example0_Mount/Main
---
###### commit `9208bcd26b1e4320aff0fd960b7c9b03c7a1be48`
###### Author: `Chris Swinchatt` <chrisswinchatt@gmail.com>
###### Date: `  Wed May 10 10:35:08 2017-2018 +0100`

    Add bytestring size checks to Volume and Block

    1. Volume will error if the bytestring is less than 2048 bytes long. This is because the superblock comprises bytes 1023:2047.
    2. Block will error if the bytestring is not a multiple of the block size, or if there are fewer blocks than reported in the superblock
---
###### commit `eff98edaa4254c10dd1f7a32ad4a8ead0e990ee9`
###### Author: `Chris Swinchatt` <chrisswinchatt@gmail.com>
###### Date: `  Wed May 10 10:35:08 2017-2018 +0100`

    Add new example

    Example0_Mount shows how to mount a filesystem and print information about it on the screen
---
###### commit `ed31f7586ae2cb7f581a05da48e5d9d7306bc3ae`
###### Author: `Chris Swinchatt` <chrisswinchatt@gmail.com>
###### Date: `  Wed May 10 10:35:08 2017-2018 +0100`

    Bug fixes

    * Show instances added for all FS objects
    * Fixed bug in Ext2.Volume.getInode
---
###### commit `3e3049dbb37fba1389576660c81c21c513d2b416`
###### Author: `Chris Swinchatt` <chrisswinchatt@gmail.com>
###### Date: `  Wed May 10 10:35:08 2017-2018 +0100`

    Add Show instances for filesystem dump
---
###### commit `a7e5ec8d445762a74981765e6b3ff51de1c4dab9`
###### Author: `Chris Swinchatt` <chrisswinchatt@gmail.com>
###### Date: `  Wed May 10 10:35:08 2017-2018 +0100`

    Implement directory open and read with user & group permissions

    File permissions checked when opening directories using user ID and groups against inode UID and GID. Owner/group/other permissions are used as appropriate according to UID, with root user (UID = 0)
    always having owner permissions.
---
###### commit `7d78754d182db1ff268754e622d6ccf89965a24c`
###### Author: `Chris Swinchatt` <chrisswinchatt@gmail.com>
###### Date: `  Wed May 10 10:35:08 2017-2018 +0100`

    Add build dependencies in Cabal file
---
###### commit `699dddfe84e44aa83207d570573e3fc355e1c448`
###### Author: `Chris Swinchatt` <chrisswinchatt@gmail.com>
###### Date: `  Wed May 10 10:35:08 2017-2018 +0100`

    Improve code style

    Code style improved based on hints from hlint
---
###### commit `32f2d0757072c21713d48ad0ecb725023b3acb4d`
###### Author: `Chris Swinchatt` <chrisswinchatt@gmail.com>
###### Date: `  Wed May 10 10:35:08 2017-2018 +0100`

    Implement file paths

    Directories search for child nodes along /-delimited paths
---
###### commit `51aff08d4edcbf564427657b4965667e624b9029`
###### Author: `Chris Swinchatt` <chrisswinchatt@gmail.com>
###### Date: `  Wed May 10 10:35:08 2017-2018 +0100`

    Implement directory tree abstraction
---
###### commit `f5e253a0a51e1c0b02e69ec394a80ed33e42449b`
###### Author: `Chris Swinchatt` <chrisswinchatt@gmail.com>
###### Date: `  Wed May 10 10:35:08 2017-2018 +0100`

    Implement reading from nodes across block boundaries
---
###### commit `3f572eb47ae5839a425ef5ed92661c44946e9244`
###### Author: `Chris Swinchatt` <chrisswinchatt@gmail.com>
###### Date: `  Wed May 10 10:35:08 2017-2018 +0100`

    Implement inode search

    Ext2.Block has been moved back into Ext2.LowLevel
---
###### commit `1afbe3820fbec3211887763a7bb1e741f707f278`
###### Author: `Chris Swinchatt` <chrisswinchatt@gmail.com>
###### Date: `  Wed May 10 10:35:08 2017-2018 +0100`

    Implement UUIDs using uuid library
---
###### commit `efeb8eaca9f6c648009a49bcf182735f5615b598`
###### Author: `Chris Swinchatt` <chrisswinchatt@gmail.com>
###### Date: `  Wed May 10 10:35:08 2017-2018 +0100`

    Implement UNIX timestamps
---
###### commit `d95575abcd1a255680069b46bfd0605045ebc394`
###### Author: `Chris Swinchatt` <chrisswinchatt@gmail.com>
###### Date: `  Wed May 10 10:35:08 2017-2018 +0100`

    Implement block-based I/O with object serialisation/deserialisation
---
###### commit `316f726909e5d0b16654c0034cda1f7d9205d8df`
###### Author: `Chris Swinchatt` <chrisswinchatt@gmail.com>
###### Date: `  Wed May 10 10:35:08 2017-2018 +0100`

    Restructure Ext2.LowLevel
---
###### commit `94fef9a52d109acbb54c383f8e5014a4caab0b63`
###### Author: `Chris Swinchatt` <chrisswinchatt@gmail.com>
###### Date: `  Wed May 10 10:35:08 2017-2018 +0100`

    Minor change to Inode structure

    Indirect blocks are now separate fields, not elements in the iBlock
    array.
---
###### commit `3e112e3cf25cc26e65839aaee79543ec5f4dd73a`
###### Author: `Chris Swinchatt` <chrisswinchatt@gmail.com>
###### Date: `  Wed May 10 10:35:08 2017-2018 +0100`

    Create SizeOf typeclass to compute the size of objects

    Ext2.LowLevel.Structures.struct now generates an instance of SizeOf
    automatically
---
###### commit `b07b57c1fbdb7410928bfcd61aab1f3b009ae544`
###### Author: `Chris Swinchatt` <chrisswinchatt@gmail.com>
###### Date: `  Wed May 10 10:35:08 2017-2018 +0100`

    Remove Ext2.LowLevel.IO

    The functionality of IO has been moved into Ext2.LowLevel.Block
---
###### commit `3b1b168f082370d8a76a73d04e182de2f5751c98`
###### Author: `Chris Swinchatt` <chrisswinchatt@gmail.com>
###### Date: `  Wed May 10 10:35:08 2017-2018 +0100`

    Allow Structs to specify their own deriving instances

    Previously all Structs were forced to derive Show, meaning this instance
    could not be overridden
---
###### commit `49a0877bc47997cdb067017b10ec8701091b2130`
###### Author: `Chris Swinchatt` <chrisswinchatt@gmail.com>
###### Date: `  Wed May 10 10:35:08 2017-2018 +0100`

    Implement low-level block device I/O
---
###### commit `0f82764e83376bd42688e55df295d298000e017e`
###### Author: `Chris Swinchatt` <chrisswinchatt@gmail.com>
###### Date: `  Wed May 10 10:35:08 2017-2018 +0100`

    Add predefined error strings
---
###### commit `ac030439ec83b06e7ed93595b6b1315c219d9818`
###### Author: `Chris Swinchatt` <chrisswinchatt@gmail.com>
###### Date: `  Wed May 10 10:35:08 2017-2018 +0100`

    Add QuickCheck test for Serialisable
---
###### commit `c6bb40e60968402143da6b9cfdbf2c5cb85b4e93`
###### Author: `Chris Swinchatt` <chrisswinchatt@gmail.com>
###### Date: `  Wed May 10 10:35:08 2017-2018 +0100`

    Generate data structures with Template Haskell
---
###### commit `48b112475311b38dd4ab3a345a5f57a9bb59f717`
###### Author: `Chris Swinchatt` <chrisswinchatt@gmail.com>
###### Date: `  Wed May 10 10:35:08 2017-2018 +0100`

    Remove code generator
     * Implement WordArray type
     * Implement Serialisable typeclass
     * Convert generated data structures into static Haskell code
     * Create Serialisable instances for structures
---
###### commit `c23da06647a5a5be95f0c15b44e1b8d9630fbee1`
###### Author: `Chris Swinchatt` <chrisswinchatt@gmail.com>
###### Date: `  Wed May 10 10:35:08 2017-2018 +0100`

    Create data structure generator
---
###### commit `c2f86857458db3c47e45760abe1f0e7162245da6`
###### Author: `Chris Swinchatt` <chrisswinchatt@gmail.com>
###### Date: `  Wed May 10 10:35:08 2017-2018 +0100`

    Initial commit
