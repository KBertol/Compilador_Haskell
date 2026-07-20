.class public teste
.super java/lang/Object

.method public <init>()V
	aload_0
	invokenonvirtual java/lang/Object/<init>()V
	return
.end method

.method public static maior(DD)D
	.limit stack 50
	.limit locals 5

	dload_0
	dload_2
	dcmpg
	ifgt L0
	goto L1
L0:
	dload_0
	d2i
	istore 4
	goto L2
L1:
	dload_2
	d2i
	istore 4
L2:
	iload 4
	i2d
	dreturn

.end method

.method public static fat(I)I
	.limit stack 50
	.limit locals 2

	iconst_0
	istore_1
L3:
	iload_0
	iconst_0
	if_icmpgt L4
	goto L5
L4:
	iload_1
	iload_0
	imul
	istore_1
	iload_0
	iconst_1
	isub
	istore_0
	goto L3
L5:
	iload_1
	ireturn

.end method

.method public static somatorio(I)I
	.limit stack 50
	.limit locals 4

	iconst_0
	i2d
	dstore_2
	iconst_0
	istore_1
L6:
	iload_1
	iload_0
	if_icmplt L7
	goto L8
L7:
	dload_2
	iload_1
	i2d
	dadd
	dstore_2
	iload_1
	iconst_1
	iadd
	istore_1
	goto L6
L8:
	dload_2
	d2i
	ireturn

.end method

.method public static imprimir(Ljava/lang/String;D)V
	.limit stack 50
	.limit locals 3

	getstatic java/lang/System/out Ljava/io/PrintStream;
	aload_0
	invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	dload_1
	invokevirtual java/io/PrintStream/println(D)V

.end method

.method public static main([Ljava/lang/String;)V
	.limit stack 50
	.limit locals 5

	getstatic java/lang/System/out Ljava/io/PrintStream;
	ldc "Numero:"
	invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V
	new java/util/Scanner
	dup
	getstatic java/lang/System/in Ljava/io/InputStream;
	invokespecial java/util/Scanner/<init>(Ljava/io/InputStream;)V
	invokevirtual java/util/Scanner/nextInt()I
	istore_2
	ldc2_w 4.5
	d2i
	invokestatic teste/fat(I)I
	istore_1
	ldc2_w 2.5
	bipush 10
	i2d
	invokestatic teste/maior(DD)D
	dstore_3
	ldc "teste:"
	iconst_1
	i2d
	invokestatic teste/imprimir(Ljava/lang/String;D)V
	return
.end method
