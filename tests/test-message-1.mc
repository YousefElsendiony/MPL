/* This test confirms structs can be created and assigned values. */

struct Packet {
  int val;
  string name;
};

struct Message {
  string messageName;
  struct Packet startPacket;
  struct Packet midPacket;
  struct Packet endPacket;

};

int main()
{
  /* Declare Struct to emulate a Communication Packets */
  struct Packet p1;
  struct Packet p2;
  struct Packet p3;
  struct Message m;

  /* Assign Values to the Communication Packet 1 */
  p1.name = "Packet 1";
  p1.val = 25;

  /* Print Values of the Communication Packet 1 */
  prints(p1.name);
  print(p1.val);

  /* Assign Values to the Communication Packet 2 */
  p2.name = "Packet 2";
  p2.val = 35;

  /* Print Values of the Communication Packet 2 */
  prints(p2.name);
  print(p2.val);

  /* Assign Values to the Communication Packet 3 */
  p3.name = "Packet 3";
  p3.val = 50;

  /* Print Values of the Communication Packet 3 */
  prints(p3.name);
  print(p3.val);

  return 0;
}
