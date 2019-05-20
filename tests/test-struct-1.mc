/* This test confirms structs can be created and assigned values. */

struct Packet {
  int val;
  string name;
};

int main()
{
  /* Declare Struct to emulate a Communication Packets */
  struct Packet p1;
  struct Packet p2;

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

  return 0;
}
