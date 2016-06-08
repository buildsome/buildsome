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
#BuildRequires: happy
BuildRequires:  gmp-devel
BuildRequires:  python
BuildRequires:  chrpath

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

mkdir -p ~/.local/{bin,stack}
export PATH=~/.local/bin:$PATH

if [[ ! -x ~/.local/bin/stack ]] ; then
    curl -L https://github.com/commercialhaskell/stack/releases/download/v1.0.4.3/stack-1.0.4.3-linux-x86_64.tar.gz | \
	tar -zxf - -C ~/.local/stack
    ln -f -s ~/.local/stack/stack-1.0.4.3-linux-x86_64/stack ~/.local/bin/stack
fi

stack --no-terminal setup

pushd ~
# A hack to make sure *some* version of Happy is installed before proceeding
stack --no-terminal setup
stack install happy
popd

%build

# It depends on git, we need to fix that. For now:
export BUILDSOME_BUILT_REVISION=%{version}-%{release}
export PATH=~/.local/bin:$PATH

# Proceed to build Buildsome
stack --no-terminal build

%install

echo rpm build root: $RPM_BUILD_ROOT
echo prefix is: %{_prefix}

mkdir -p $RPM_BUILD_ROOT/%{_prefix}/share/doc/buildsome-0.1.0.0
cp LICENSE.txt $RPM_BUILD_ROOT/%{_prefix}/share/doc/buildsome-0.1.0.0/LICENSE.txt

mkdir -p $RPM_BUILD_ROOT/%{_prefix}/share/buildsome-0.1.0.0/cbits
cp .stack-work/*/*/*/*/*/*/*/cbits/fs_override.so $RPM_BUILD_ROOT/%{_prefix}/share/buildsome-0.1.0.0/cbits

mkdir -p $RPM_BUILD_ROOT/%{_prefix}/bin
buildsome_exe=$RPM_BUILD_ROOT/%{_prefix}/bin/buildsome
cp .stack-work/install/*/*/*/bin/buildsome ${buildsome_exe}

abs_share_path=$(ls -1d `pwd`/.stack-work/*/*/*/*/*/*/buildsome-0.1.0.0)

# Patch executable to point to the correct path of fs_override.so (how are
# you supposed to do this with Stack? Did not find in docs.

python -c """
f = \"${buildsome_exe}\"
content = open(f).read()
a = \"${abs_share_path}\"
b = \"%{_prefix}/share/buildsome-0.1.0.0\"
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
%{_prefix}/share/buildsome-0.1.0.0/cbits/fs_override.so
%{_prefix}/share/doc/buildsome-0.1.0.0/LICENSE.txt

%changelog
