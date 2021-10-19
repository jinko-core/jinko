#!/bin/sh

echo "Run this script in order to create the tarball containing a jinko release"
echo "The first and only argument needs to be the tag you'd like to give to the release,"
echo "i.e. \`./make_release.sh 0.2.0-jinx3\`"
echo "Only run this script from the root of the project. It's not really smart..."

if [ $# -ne 1 ]; then
    exit 1
fi

# We assume that the tag will already have been created...

echo "Compiling project in release mode..."
cargo build --release

# Create a temporary directory
tmp_dir=$(mktemp -d)

mkdir $tmp_dir/jinko-$1

echo "Copying stdlib..."
cp -r stdlib $tmp_dir/jinko-$1

echo "Copying binary..."
cp target/release/jinko $tmp_dir/jinko-$1

echo "Copying install.sh..."
cp install.sh $tmp_dir/jinko-$1

echo "Compressing release..."
old_pwd=$PWD
cd $tmp_dir

tar czf jinko-$1.tar.gz jinko-$1/*

cd $old_pwd
mv $tmp_dir/jinko-$1.tar.gz .

echo "Cleaning up..."
rm -rf $tmp_dir

echo "Created release: jinko-$1.tar.gz"
