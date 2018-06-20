{ stdenv, buildPythonApplication, libnl-python }:

buildPythonApplication {

  name="mptcp_netlink";
  version="0.1";

  src=./.;

  propagatedBuildInputs = [ libnl-python ];
  installPhase = ''
    mkdir -p $out/bin
    cp daemon/daemon.py $out/bin/mptcp-pmd
  '';

  # propagatedBuildInputs = [];

  meta = with stdenv.lib; {
    # or mptcpnetlink
    homepage = http://github.com/teto/netlink_pm;
    description = "python daemon acting as a netlink MPTCP path manager";
    license = licenses.gpl3;
    maintainers = with maintainers; [ teto ];
  };


}
