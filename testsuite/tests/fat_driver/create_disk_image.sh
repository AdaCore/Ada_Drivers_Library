#!/bin/bash

# This script creates a FAT disk image containing files in different
# directories and sub-directories. The files contain random data and are named
# with the md5 hash of their content.
#
# This disk image is meant to be used for FAT driver testing. The test should
# look for all the file in the 'read_test' sub-tree and compare the file name
# with the md5 hash of it's content.
#
# There's also a file named 'number_of_files_to_check' that the test read to
# check that it processed all the required files.


number_of_files=0

create_test_file() {
    dir=$1
    tmp_file=$dir/tmp
    dd if=/dev/urandom of=$tmp_file bs=1 count=$(((RANDOM % 200 ) + 500))
    md5=`md5sum $tmp_file | cut -d' ' -f1`
    echo $md5
    mv $tmp_file $dir/$md5
    number_of_files=$(($number_of_files + 1))
}

create_test_dir() {
    dir=$1
    mkdir $dir
    create_test_file $dir
}

SIZE=64
dd if=/dev/zero of=fat.fs bs=1024 count=$SIZE

mkfs.vfat -F 32 fat.fs
mkdir mnt
fusefat -o rw+ fat.fs mnt
sleep 1

mkdir mnt/read_test
create_test_file mnt/read_test/
create_test_dir  mnt/read_test/lvl1_a
create_test_dir  mnt/read_test/lvl1_b
create_test_dir  mnt/read_test/lvl1_b/lvl2_a_directory_with_a_long_name_to_check_if_this_is_correctly_handled_by_the_fat_driver
create_test_dir  mnt/read_test/lvl1_c
create_test_dir  mnt/read_test/lvl1_c/lvl2_b
create_test_dir  mnt/read_test/lvl1_c/lvl2_b/lvl3_a
echo $number_of_files > mnt/number_of_files_to_check

tree -h mnt
fusermount -u mnt
rmdir mnt
