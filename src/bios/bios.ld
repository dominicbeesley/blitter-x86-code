MEMORY {
	mainrom : ORIGIN = 0, LENGTH = 0x3FF0
	vectors : ORIGIN = 0x3FF0, LENGTH = 0x10
}


SECTIONS
{
	/* Load the kernel at this address: "." means the current address */
	. = 0xFC00;

	.text : {
		*(.text .stub .text.* .gnu.linkonce.t.*)
	} AT> mainrom

	. = 0xFFFF;

	.RESETVEC : {
		*(.RESETVEC)
	} AT> vectors

}
