# Simulate communication failure

The external controller needs the communication to the backend to receive jobs and routes. Nevertheless the system can handle a disconnection  to the backend. To simulate this communication failure, the firewall of the computer can be used.

- Open "Windows Defender Firewall" (Control Panel -> System and Security -> Windows Defender Firewall)
- Open "Advanced Settings"
- Right click "Outbound Rules" -> Add new Rule

- Choose "Port" -> Next

- Choose "TCP" , specific remote ports: "8081" -> Next
- Leave everything on default (Block the connection) -> Next
- Leave everything on default (All choices set) -> Next
- Type in a suitable name for example "Block the Backend Connection Cab Service 1"

- You will find a new a new Entry in the "Outbound Rules" Tab.

- You can reopen the rule with a double click and choose between Allow/Block the connection.