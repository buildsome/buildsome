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
# Instead, it gets some of its dependencies from a special 'buildsome-deps'.
# This can be rectified in the future.

BuildRequires:  buildsome-deps
BuildRequires:  ghc-Cabal
BuildRequires:  cabal-install

BuildRequires:  ghc-Cabal-devel
BuildRequires:  ghc-transformers-devel
BuildRequires:  ghc-mtl-devel
BuildRequires:  ghc-text-devel
BuildRequires:  ghc-random-devel
BuildRequires:  ghc-template-haskell-devel

BuildRequires:  leveldb-devel
BuildRequires:  snappy-devel

%description
Buildsome is an innovative build system, meant to both ease the
declaration of the build steps, and give better guarantees to users.

%prep
%setup -q

%build

# It depends on git, we need to fix that. For now:
export BUILDSOME_BUILT_REVISION=%{version}-%{release}
cabal configure --flags="CentOS7" --prefix=%{_prefix} \
   --package-db /usr/share/buildsome-deps/.cabal-sandbox/*-packages.conf.d/
cabal build

%install
cabal copy --destdir=$RPM_BUILD_ROOT

%files
%{_prefix}/bin/buildsome
# FIXME: Version needs to be automated here.
%{_prefix}/share/buildsome-0.1.0.0/cbits/fs_override.so
%{_prefix}/share/doc/buildsome-0.1.0.0/LICENSE.txt

%changelog
