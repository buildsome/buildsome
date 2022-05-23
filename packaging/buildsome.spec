Name:           buildsome
Version:        PKG_VERSION
Release:        PKG_RELEASE%{?dist}
Summary:        Summary

Group:          System Environment/Libraries
License:        GPLv2
URL:            https://github.com/ElastiLotem/buildsome
Source0:        %{name}-%{version}.tar.gz
BuildRoot:      %(mktemp -ud %{_tmppath}/%{name}-%{version}-%{release}-XXXXXX)

# NOTE: This spec does not adhere to GHC packaging under Fedora.
# Instead, it uses Stack from the internet.

# This can be rectified in the future.
BuildRequires:  leveldb-devel
BuildRequires:  snappy-devel
BuildRequires:  gmp-devel
BuildRequires:  python3
BuildRequires:  python2
BuildRequires:  chrpath
BuildRequires:  wget
BuildRequires:  perl
BuildRequires:  make
BuildRequires:  automake
BuildRequires:  gcc
BuildRequires:  gmp-devel
BuildRequires:  libffi
BuildRequires:  zlib
BuildRequires:  zlib-devel
BuildRequires:  xz
BuildRequires:  tar
BuildRequires:  git
BuildRequires:  gnupg
BuildRequires:  ncurses-libs

%if 0%{?fedora} >= 24
# GHC builds need tinfo.so.5
BuildRequires:  ncurses-compat-libs
BuildRequires:  glibc-langpack-en
%endif

Requires:       gmp

%description
Buildsome is an innovative build system, meant to both ease the
declaration of the build steps, and give better guarantees to users.

%global debug_package %{nil}

%prep
%setup -q

mkdir -p ~/.local/{bin,lib}
export PATH=~/.local/bin:$PATH
export LD_LIBRARY_PATH=~/.local/lib:$LD_LIBRARY_PATH
ln -s /usr/lib64/libncursesw.so.6 ~/.local/lib/libncursesw.so.5
cd ~/.local
wget http://archive.ubuntu.com/ubuntu/pool/main/g/gmp/gmp_6.1.2+dfsg.orig.tar.xz
tar xf gmp_6.1.2+dfsg.orig.tar.xz
cd gmp-6.1.2+dfsg
./configure  --prefix=$HOME/.local
make install
ln -s ~/.local/lib/libgmp.so.10 ~/.local/lib/libgmp.so.3
cd ~/.local
wget https://downloads.haskell.org/~ghc/7.0.1/ghc-7.0.1-x86_64-unknown-linux.tar.bz2
tar xf ghc-7.0.1-x86_64-unknown-linux.tar.bz2
cd ghc-7.0.1
./configure --prefix=$HOME/.local
make install
cd ~/.local
curl -sSL https://get.haskellstack.org/ -o haskellstack.sh
bash ./haskellstack.sh -d ~/.local/bin
stack install alex happy


%build

# It depends on git, we need to fix that. For now:
export BUILDSOME_BUILT_REVISION=%{version}-%{release}
export PATH=~/.local/bin:$PATH
export LD_LIBRARY_PATH=~/.local/lib:$LD_LIBRARY_PATH

# Proceed to build Buildsome
stack --no-terminal build

%install

echo rpm build root: $RPM_BUILD_ROOT
echo prefix is: %{_prefix}

mkdir -p $RPM_BUILD_ROOT/%{_prefix}/share/doc/buildsome-0.1.1.0
cp LICENSE.txt $RPM_BUILD_ROOT/%{_prefix}/share/doc/buildsome-0.1.1.0/LICENSE.txt

mkdir -p $RPM_BUILD_ROOT/%{_prefix}/share/buildsome-0.1.1.0/cbits
cp .stack-work/*/*/*/*/*/*/*/cbits/fs_override.so $RPM_BUILD_ROOT/%{_prefix}/share/buildsome-0.1.1.0/cbits

mkdir -p $RPM_BUILD_ROOT/%{_prefix}/bin
buildsome_exe=$RPM_BUILD_ROOT/%{_prefix}/bin/buildsome
cp .stack-work/install/*/*/*/bin/buildsome ${buildsome_exe}

abs_share_path=$(ls -1d `pwd`/.stack-work/*/*/*/*/*/*/buildsome-0.1.1.0)

# Patch executable to point to the correct path of fs_override.so (how are
# you supposed to do this with Stack? Did not find in docs.

python2 -c """
f = \"${buildsome_exe}\"
content = open(f).read()
a = \"${abs_share_path}\"
b = \"%{_prefix}/share/buildsome-0.1.1.0\"
b += \"\0\" * (len(a) - len(b))
content = content.replace(a, b)
open(f, \"w\").write(content)
"""

chmod a+x ${buildsome_exe}

# Remove RPATH absolute references to build dir.
chrpath -d ${buildsome_exe}

%files
%{_prefix}/bin/buildsome
# FIXME: Version needs to be automated here.
%{_prefix}/share/buildsome-0.1.1.0/cbits/fs_override.so
%{_prefix}/share/doc/buildsome-0.1.1.0/LICENSE.txt

%changelog
