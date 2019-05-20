int main()
{
  /* Declarations of type byte */
  byte a;
  byte b;
  byte c;

 /* Declares an Array of size 3 */
  int[3] packetA;

  /* Assigns the Array Initial values */
  packetA = [3,2,1];

  prints("Example of Byte and Raw operation.");
  raw(15);
  raw(80);
  raw(255);
  raw(packetA);
  print($15);
  print($80);
  print($255);

  return 0;
}
