%define __jar_repack %{nil}
%define debug_package %{nil}
%define __strip /bin/true
%define __os_install_post   /bin/true
%define __check_files /bin/true
Summary: panopticon
Name: panopticon
Version: 0.1.0
Release: 1
Epoch: 0
BuildArchitectures: noarch
Group: Applications
BuildRoot: %{_tmppath}/%{name}-%{version}-buildroot
License: BSD
Provides: panopticon
Source0: %{name}-%{version}.tar.gz

%description
iPlant Panopticon

%pre
getent group iplant > /dev/null || groupadd -r iplant
getent passwd iplant > /dev/null || useradd -r -g iplant -d /home/iplant -s /bin/bash -c "User for the iPlant services." iplant
exit 0

%prep
%setup -q
mkdir -p $RPM_BUILD_ROOT/etc/init.d/

%build
unset JAVA_OPTS
lein deps
lein uberjar

%install
install -d $RPM_BUILD_ROOT/usr/local/lib/panopticon/
install -d $RPM_BUILD_ROOT/var/run/panopticon/
install -d $RPM_BUILD_ROOT/var/lock/subsys/panopticon/
install -d $RPM_BUILD_ROOT/var/log/panopticon/
install -d $RPM_BUILD_ROOT/etc/panopticon/

install panopticon $RPM_BUILD_ROOT/etc/init.d/
install panopticon-1.0.0-SNAPSHOT-standalone.jar $RPM_BUILD_ROOT/usr/local/lib/panopticon/
install conf/log4j.properties $RPM_BUILD_ROOT/etc/panopticon/
install conf/panopticon.properties $RPM_BUILD_ROOT/etc/panopticon/

%post
/sbin/chkconfig --add panopticon

%preun
if [ $1 -eq 0 ] ; then
	/sbin/service panopticon stop >/dev/null 2>&1
	/sbin/chkconfig --del panopticon
fi

%postun
if [ "$1" -ge "1" ] ; then
	/sbin/service panopticon condrestart >/dev/null 2>&1 || :
fi

%clean
lein clean
rm -r lib/*

%files
%attr(-,iplant,iplant) /usr/local/lib/panopticon/
%attr(-,iplant,iplant) /var/run/panopticon/
%attr(-,iplant,iplant) /var/lock/subsys/panopticon/
%attr(-,iplant,iplant) /var/log/panopticon/
%attr(-,iplant,iplant) /etc/panopticon/

%config %attr(0644,iplant,iplant) /etc/panopticon/log4j.properties
%config %attr(0644,iplant,iplant) /etc/panopticon/panopticon.properties

%attr(0755,root,root) /etc/init.d/panopticon
%attr(0644,iplant,iplant) /usr/local/lib/panopticon/panopticon-1.0.0-SNAPSHOT-standalone.jar


