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

mkdir -p $RPM_BUILD_ROOT/%{_prefix}/bin
cp .stack-work/install/*/*/*/bin/buildsome $RPM_BUILD_ROOT/%{_prefix}/bin/buildsome

mkdir -p $RPM_BUILD_ROOT/%{_prefix}/share/buildsome-0.1.0.0/cbits
cp .stack-work/*/*/*/*/*/*/*/cbits/fs_override.so $RPM_BUILD_ROOT/%{_prefix}/share/buildsome-0.1.0.0/cbits

%files
%{_prefix}/bin/buildsome
# FIXME: Version needs to be automated here.
%{_prefix}/share/buildsome-0.1.0.0/cbits/fs_override.so
%{_prefix}/share/doc/buildsome-0.1.0.0/LICENSE.txt

%changelog
