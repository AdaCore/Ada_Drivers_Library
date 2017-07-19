#!/bin/bash

create_test_file() {
    dir=$1
    tmp_file=$dir/tmp
    dd if=/dev/urandom of=$tmp_file bs=1 count=$(((RANDOM % 200 ) + 500))
    md5=`md5sum $tmp_file | cut -d' ' -f1`
    echo $md5
    mv $tmp_file $dir/$md5
}

create_test_dir() {
    dir=$1
    mkdir $dir
    create_test_file $dir
}

SIZE=64
dd if=/dev/zero of=fat.fs bs=1024 count=$SIZE

# (
# echo o # Create a new empty DOS partition table
# echo n # Add a new partition
# echo p # Primary partition
# echo 1 # Partition number
# echo   # First sector (Accept default: 1)
# echo   # Last sector (Accept default: varies)
# echo t
# echo 6
# echo w # Write changes
# ) | fdisk fat.fs

mkfs.vfat fat.fs
mkdir mnt
fusefat -o rw+ fat.fs mnt
sleep 1

create_test_file mnt/
create_test_dir mnt/lvl1_a
create_test_dir mnt/lvl1_b
create_test_dir mnt/lvl1_b/lvl2_a
create_test_dir mnt/lvl1_c
create_test_dir mnt/lvl1_c/lvl2_b
create_test_dir mnt/lvl1_c/lvl2_b/lvl2_c
tree -h mnt
fusermount -u mnt
rmdir mnt
