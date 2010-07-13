///----------------------------------------------------------------------
/// Copyright (c) 2010 Gemini Mobile Technologies, Inc.  All rights reserved.
/// 
/// Licensed under the Apache License, Version 2.0 (the "License");
/// you may not use this file except in compliance with the License.
/// You may obtain a copy of the License at
/// 
///     http://www.apache.org/licenses/LICENSE-2.0
/// 
/// Unless required by applicable law or agreed to in writing, software
/// distributed under the License is distributed on an "AS IS" BASIS,
/// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
/// See the License for the specific language governing permissions and
/// limitations under the License.
///
/// File    : HibariTest.java
/// Purpose : Hibari Test
///----------------------------------------------------------------------

package ubf;

import java.net.Socket;
import java.io.*;
import ubf.UBFClient;

public class HibariTest
{
    public static void main(String[] args)
	throws Exception
    {
	Socket sock = null;
	UBFClient ubf = null;

	try {
	    sock = new Socket(args[0], 7581);
	    ubf = UBFClient.new_via_sock(new UBFString("gdss"), new UBFList(),
		    new FooHandler(), sock);
	}
	catch (Exception e) {
	    System.out.println(e);
	    System.exit(1);
	}

	test_hibari_basics(ubf);

	ubf.stopSession();
	System.out.println("Success, it works");
	System.exit(0);
    }

    public static class FooHandler implements UBFEventHandler {
    /* class UBFEventHandler { */
	public FooHandler() {
	/* public UBFEventHandler() { */
	}
	public void handleEvent(UBFClient client, UBFObject event) {
	    System.out.println("Hey, got an event: " + event.toString());
	}
	public void connectionClosed(UBFClient client) {
	    System.out.println("Hey, connection closed, ignoring it\n");
	}
    }

    public static void test_hibari_basics(UBFClient ubf)
	throws IOException, UBFException
    {
	UBFObject atom_tab1 = new UBFAtom("tab1");
	UBFObject atom_ok = new UBFAtom("ok");
	UBFObject atom_key_exists = new UBFAtom("key_exists");

	// setup
	UBFObject res1 = ubf.rpc(
		UBF.tuple( new UBFAtom("delete"), atom_tab1,
			    new UBFBinary("foo"), new UBFList(),
			    new UBFInteger(4000)));
	System.out.println("Res 1:" + res1.toString());

	// add - ok
	UBFObject res2 = ubf.rpc(
		UBF.tuple( new UBFAtom("add"), atom_tab1,
			    new UBFBinary("foo"), new UBFBinary("bar"),
			    new UBFInteger(0), new UBFList(),
			    new UBFInteger(4000)));
	System.out.println("Res 2:" + res2.toString());
	if (! res2.equals(atom_ok))
	    System.exit(1);

	// add - ng
	UBFObject res3 = ubf.rpc(
		UBF.tuple( new UBFAtom("add"), atom_tab1,
			    new UBFBinary("foo"), new UBFBinary("bar"),
			    new UBFInteger(0), new UBFList(),
			    new UBFInteger(4000)));
	System.out.println("Res 3:" + res3.toString());
	if (! ((UBFTuple)res3).value[0].equals(atom_key_exists))
	    System.exit(1);

	// get - ok
	UBFObject res4 = ubf.rpc(
		UBF.tuple( new UBFAtom("get"), atom_tab1,
			    new UBFBinary("foo"), new UBFList(),
			    new UBFInteger(4000)));
	System.out.println("Res 4:" + res4.toString());
	if (! ((UBFTuple)res4).value[0].equals(atom_ok) ||
	    ! ((UBFTuple)res4).value[2].equals("bar"))
	    System.exit(1);
	
	// set - ok
	UBFObject res5 = ubf.rpc(
		UBF.tuple( new UBFAtom("set"), atom_tab1,
			    new UBFBinary("foo"), new UBFBinary("bar bar"),
			    new UBFInteger(0), new UBFList(),
			    new UBFInteger(4000)));
	System.out.println("Res 5:" + res5.toString());
	if (! res5.equals(atom_ok))
	    System.exit(1);

	// get - ok
	UBFObject res6 = ubf.rpc(
		UBF.tuple( new UBFAtom("get"), atom_tab1,
			    new UBFBinary("foo"), new UBFList(),
			    new UBFInteger(4000)));
	System.out.println("Res 6:" + res6.toString());
	if (! ((UBFTuple)res6).value[0].equals(atom_ok) ||
	    ! ((UBFTuple)res6).value[2].equals("bar bar"))
	    System.exit(1);
	
    }
}

