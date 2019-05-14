%define sys_write 4
%define sys_open 5
%define sys_close 6
%define sys_read 3
%define READ_ONLY 0
%define WRITE_ONLY 1
%define sys_exit 1
%define stdout 1
%define BUFFSIZE 5000
%define EOF 0

section .data
	ayuda_h db '-h',0		;con el fin de agilizar la comparacion
	dpuntos db ': ',0
	final db 'Cantidad de lineas: ',0
	
	;---- Almaceno el contenido del mensaje de ayuda ----
	helpMsg db  	"Mensaje de ayuda:",10,
		db	"	Este programa toma un archivo y escribe por pantalla,o en un archivo su",10,
		db	" contenido con las lineas numeradas.",10,10,
		db	"	Sintaxis: ",10,10,
		db	"	enum [ -h ] | archivo entrada [ archivo salida]",10,
		db	"Los parametros entre corchetes son opcionales, las barras | indican",10,
		db	"opciones distintas a ingresar:",10,
		db	"		-Si ingresa '-h' se muestra la ayuda por pantalla.",10,
		db	"		-Si ingresa el archivo de entrada pero no el archivo de salida",10,
		db	"se escribe por pantalla el contenido del archivo con los renglones numerados.",10,
		db	"		-Si en cambio ingresa tambien el archivo de salida, se escribe",10,
		db	" en el archivo, el contenido del de entrada pero con las lineas numeradas.",10	
	longHelpMsg equ $ - helpMsg ;Almaceno el tamaño del mensaje de ayuda.

	nerror db 0xa,'Terminacion normal.',0xa,0xa
	lnerror equ $ - nerror
	error1 db 0xa,'Terminacion anormal por error en el archivo de entrada.',0xa,0xa
	lerror1 equ $ - error1
	error2 db 0xa,'Terminacion anormal por error en el archivo de salida.',0xa,0xa
	lerror2 equ $ - error2
	error3 db 0xa,'Terminacion anormal por otras causas.',0xa,0xa
	lerror3 equ $ - error3
	retorno db 0xa

section .bss
	buffer resb BUFFSIZE		;buffer donde leo el archivo entero
	bufferdestino resb BUFFSIZE	;buffer donde voy guardando el archivo leido a medida que lo modifico
	saveflag resb 1			;byte de flag, se utiliza para saber si mostrar por consola la informacion o guardarla en el archivo destino

section .text
	global _start;

_start:
	pop eax			;cantidad de argumentos
	pop ebx 		;nombre de programa, descarto
	cmp eax, 1		;caso en que no tenga argumentos
	je cero_arg		;salto a cero argumentos de entrada
	cmp eax, 2		;caso en que tenga un solo argumento de entrada
	je un_arg		;salto a un solo argumento de entrada
	cmp eax, 3		;caso en que tenga dos argumentos de entrada
	je dos_arg		;salto a dos argumentos
	push 3
	jmp salir		;terminacion del programa

cero_arg:
	push 3
	jmp salir		;terminacion del programa en caso de no haber parametros ingresados esto es considerado un error y debe notificarse

;.......::::::: UN PARAMETRO :::::::.......
un_arg:
	pop eax 		;puntero al primer parametro
	mov ebx, ayuda_h	;puntero al '-h'
	mov [saveflag], BYTE 0	;indico que NO tengo que grabar el archivo modificado
	jmp comp_string		;pregunto si es la ayuda

continuar:			;continuo con la ejecucion normal
	je imprimir_ayuda	;si es igual a -h salto a imprimir ayuda
	call abrir_archivo	;abro el archivo para luego mostrarlo
	push 0			;no hubo errores
	jmp salir	
;.......::::::: FIN UN PARAMETRO :::::::.......	

;.......::::::: IMPRESION DEL MENSAJE DE AYUDA :::::::.......
imprimir_ayuda:
	;Imprime por pantalla el manual de usuario
	;helpMsg son los renglones de texto a imprimir (cuenta como un solo string).
	;longhelpMsg es la longitud de dichos renglones. 
	;Utiliza la rutina imprimir para mostrar en pantalla las ayudas
	mov ECX, helpMsg
	mov EDX, longHelpMsg
	call imprimir
	push 0			;no hay errores
	jmp salir		;exit del sistema
;.......::::::: FIN IMPRESION DEL MENSAJE DE AYUDA :::::::.......

;.......::::::: RUTINA COMPARADORA DE "STRINGS" :::::::.......
;ENTRADA: 
	;EAX, primer "string" a comparar.
	;EBX, segundo "string" a comparar.
;SALIDA: 
	;ZF prendido si las cadenas son iguales
comp_string:
	;recibe un string en EAX, y otro string en EBX y los compara, el resultado se manifiesta en ZF.
	push EAX
	push EBX
	mov CL, BYTE [EAX]		;Recupero el primer caracter de EAX accediendo al puntero
	mov DL, BYTE [EBX]		;recupero el primer caracter de EBX
	cmp CL,DL			;Comparo caracter a caracter
	jne comp_fin			;Si son distintos termino
	cmp CL,0			;Veo si es el caracter nulo
	je comp_fin			;Si es el caracter nulo llegue al final del string con una comparacion exitosa
	inc EAX				;Si no es el caracter nulo, paso al siguiente caracter de EAX incrementando el valor del puntero
	inc EBX				;Paso al siguiente caracter de EBX incrementando el valor del puntero
	jmp comp_string			;Hago una nueva iteracion con los indices avanzados al proximo caracter

comp_fin:
	pop EBX
	pop EAX
	jmp continuar
;.......::::::: FIN RUTINA COMPARADORA DE "STRINGS" :::::::.......

;.......::::::: RUTINA DE IMPRESION :::::::.......
;ENTRADA: 
	;ECX, buffer a imprimir.
	;EDX, largo del buffer.
;SALIDA: 
	;se imprime el buffer comenzando en el puntero ECX en la consola.
imprimir:
	;El metodo ya recibe el buffer a imprimir y la cantidad de bytes a imprimir en los registros correspondientes
	mov EAX, sys_write			;Quiero imprimir
	mov EBX, stdout				;por STDOUT, consola
	int 0x80				;Ejecuto la interrupcion al procesador
	ret
;.......::::::: FIN RUTINA DE IMPRESION :::::::.......

;.......::::::: RUTINA DE PROCESAMIENTO DE ARCHIVO :::::::.......
;ENTRADA: 
	;EAX, ruta del archivo en el sistema.
abrir_archivo:
	;EAX contiene la ruta del archivo, como no se necesita luego puedo destruir EAX.
	;abre el archivo en EAX y guarda su contenido en la direccion buffer, que actuara de buffer temporal.
	;Una vez leido el archivo cuenta las lineas a medida que copia todo el contenido del buffer a la direccion bufferdestino,
	;que actuara de receptor del archivo modificado. Dicha modificacion consiste de agregar el numero de linea al inicio de cada linea
	;y finalizado el archivo se agrega al final la cantidad de lineas contadas.
	mov EBX, EAX				;Muevo la ruta del archivo a EBX
	mov EAX, sys_open			;Quiero abrir un archivo existente
	mov ECX, READ_ONLY			;Modo solo lectura
	mov EDX, 644o				;permiso rw- r-- r--
	int 80h					;ejecuto el sys_open
	cmp EAX, 0				;testeo si EAX es negativo (error), el descriptor de archivo
	jg cont					;si no hay error sigo
	push 1
	jl salir				;Si es negativo no se pudo abrir el archivo por lo que debo mostrar el mensaje acorde y detener el programa
cont:
	push EBX				;almaceno la ruta del archivo para cerrarlo cuando termine
	
	mov EBX,EAX				;muevo el descriptor de archivo a EBX para preparar la lectura
	mov EAX,sys_read			;Quiero leer un archivo
	mov ECX,buffer				;en el buffer
	mov EDX,BUFFSIZE			;la mayor cantidad de numeros que pueda hasta encontrar un EOF (caracter nulo), o tamaño maximo del buffer
	int 80h					;ejecuto la operacion sys_read, va a cargar el archivo en buffer
						;en este punto buffer es un puntero al inicio del archivo (un string largo conteniendo todo el archivo)
						;debo usar buffermodificado para guardar una copia del archivo leido pero con las lineas contadas.
	jmp contar_lineas			;llamo a la rutina para procesar el archivo

seguir:
	mov ECX, bufferdestino
	mov EDX, BUFFSIZE
	call imprimir				;llamo a la rutina, va a imprimir el contenido del buffer leido del archivo ya modificado.

fin_escritura:					;en caso de que no tenga que mostrar por pantalla, sino que guardar en un segundo archivo
	mov EAX,sys_close			;cierro el archivo
	pop EBX					;recupero nombre del archivo guardado 
	int 80h					;llamo a sys_close
	
	ret					;retorno al programa

contar_lineas:
	mov EBX, buffer				;archivo
	mov EAX, bufferdestino 			;buffer destino
	xor ECX, ECX				;limpio ECX para usarlo de contador
	xor EDX, EDX				;limpio EDX para usarlo de destino de lectura de caracter del buffer
	mov ECX, 0				;inicializo el contador
						;rol hasta que cmp CL sea 0 contando cuantas veces paso, y hasta que sea 0 el contador hago push CL +48, despues popeo en [EAX]
outer:						;lease "resetear" el proceso de lectura de lineas, desde aca hasta "loop" muevo al buffer destino el contador y los dos puntos.
	;inc ECX 
	;mov [EAX], BYTE CL			;"grabo" el numero en el contador en el buffer destino.
	;inc EAX				;incremento el puntero del buffer destino
	inc ECX					;incremento el numero a transformar
	push ECX				;resguardo los registros, caller save, no necesito guardar EAX, devuelve en ESI el puntero actualizado
	push EBX				;
	push EDX				;
	call transformar			;convierto ECX a ascii
	pop EDX
	pop EBX
	pop ECX
	mov EAX, ESI				;devuelvo el puntero al archivo que quedo en ESI al registro EAX
	mov [EAX], BYTE 58			;copio los dos puntos de dpuntos e incremento el puntero
	inc EAX
	mov [EAX], BYTE 32
	inc EAX

inner:
	cmp BYTE [EBX], 0			;pregunto si estoy ante el fin de linea y corto el bucle
	jz finalizar_modif	
	mov DL, BYTE [EBX]			;guardo el primer caracter en un registro
	mov [EAX], BYTE DL			;guardo el caracter en el bufferdestino e incremento los punteros
	inc EBX
	inc EAX
	cmp BYTE [EBX], 0xa			;pregunto si termino la linea
	jnz inner				;si no termine de procesar la linea continuo		
	mov BYTE [EAX], 0xa			;sigo recursion
	inc EAX
	inc EBX
	jmp outer

finalizar_modif:
	mov [EAX], BYTE 0xa
	inc EAX					;posiciono el puntero en el ultimo elemento vacio del mismo
	mov EBX, final				;muevo la linea de 'cantidad de lineas'
	mov BYTE [EAX], 0xa			;imprimo una linea separadora
	inc EAX
finalup:
	cmp BYTE [EBX], 0			;pregunto si estoy ante el fin de linea y corto el bucle
	jz finalizar_modif	
	mov DL, BYTE [EBX]			;guardo el primer caracter en un registro
	mov [EAX], BYTE DL			;guardo el caracter en el bufferdestino e incremento los punteros
	inc EBX
	inc EAX
	cmp BYTE [EBX], 0			;pregunto si termino el texto
	jnz finalup				;si no termine de procesar la linea continuo
				;grabo el contador de nuevo
	push ECX				;resguardo los registros, caller save, no necesito guardar EAX, devuelve en ESI el puntero actualizado
	push EBX				;
	push EDX				;
	call transformar			;convierto ECX a ascii
	pop EDX
	pop EBX
	pop ECX
	mov EAX, ESI				;devuelvo el puntero al archivo que quedo en ESI al registro EAX
				;fin grabado
	mov BYTE [EAX], 46			;sigo recursion
	inc EAX
	mov BYTE [EAX], 0			;imprimo una linea separadora
	cmp [saveflag], BYTE 4			;pregunto si tengo que mostrar por pantalla o no el archivo.
	jz fin_escritura
	jnz seguir				;vuelvo a donde se llamo esta rutina, abrir_archivo
;.......::::::: FIN RUTINA DE PROCESAMIENTO DE ARCHIVO :::::::.......


;.......::::::: RUTINA TRANSFORMACION DE UN ENTERO EN UN REGISTRO A ASCII (CARACTER) :::::::.......
;ENTRADA:
	;EAX: puntero a archivo.
	;ECX: numero a desarmar. El numero no puede contener mas de 3 digitos, debido al metodo de division elegido (BYTE).
;SALIDA:
	;ESI: puntero a archivo actualizado.
transformar:
	mov ESI,EAX		;guardo el puntero al bufferdestino, que queda en EAX al llamar a este metodo
	xor EAX, EAX		;limpio el registro donde divido
	mov EAX,ECX		;guardo el numero a desarmar		
	xor ECX, ECX		;limpio registros a utilizar
	xor EBX, EBX		;
	xor EDX, EDX		;
	mov DL, BYTE 10		;guardo el divisor en DL (AL:= AX/DL AH:=Resto)
cciclo:
	div DL			;computo la division
	add AH, BYTE 48		;tiene la unidad en ascii
	mov CL, AH		;resguardo la unidad en la pila
	push ECX		;resguardo la unidad en la pila
	inc EBX			;incremento la cantidad de numeros
	mov AH, BYTE 0		;limpio el resto hallado
	cmp AL, BYTE 0		;AL destino implicito del resultado, es el numero sin la unidad, AH tiene la unidad (resto)
	jz finish		;me muevo si es cero a finish
	jmp cciclo		;vuelvo a ciclar si la division no dio cero

finish:
	xor ECX, ECX		;limpio el receptor del numero
	pop ECX			;obtengo un numero
	mov [ESI], ECX		;lo guardo en memoria (EAX bufferdestino)
	dec EBX			;decremento la cantidad de unidades
	inc ESI			;incremento el puntero al bufferdestino
	cmp EBX, 0		;si no es cero (no consumi todos los numeros de la pila) ciclo
	jnz finish
	ret
;.......::::::: FIN TRANSFORMAR UN ENTERO EN UN REGISTRO A ASCII :::::::......


;.......::::::: DOS PARAMETROS :::::::.......
dos_arg:
	mov [saveflag], BYTE sys_write		;indico que SI tengo que grabar el archivo modificado
	pop EAX					;abrir_archivo recibe la ruta del archivo en EAX
	call abrir_archivo			;llamo a la rutina para procesar un archivo, se guarda en bufferdestino
	pop EBX					;obtengo la ruta del archivo destino para la rutina save
	mov EAX, bufferdestino			;muevo el puntero al buffer para la rutina save
	call save				;rutina de grabado a archivo
	push 0					;salir sin errores
	jmp salir
;.......::::::: FIN DOS PARAMETROS :::::::.......

;.......::::::: GRABAR EN UN ARCHIVO UN BUFFER :::::::.......
;ENTRADA: 
	;EAX, buffer que grabar.
	;EBX, ruta del archivo donde grabar.
;SALIDA:
	;se graba el buffer indicado en EAX en el archivo cuya ruta fue indicada en EBX

save:			;APERTURA DE ARCHIVO
	push EBX				;salvo la direccion del archivo para su cierre mas adelante
	push EAX				;salvo en la pila la direccion del buffer
	mov EAX, sys_open			;Quiero abrir un archivo existente
	mov ECX, WRITE_ONLY			;Modo escritura
	mov EDX, 644o				;permiso rw- r-- r--
	int 80h					;ejecuto el sys_open
	cmp EAX, BYTE 0				;testeo si EAX es negativo (error), el descriptor de archivo
	jg escrt				;no hay errores
	push 2					;erro de apertura de archivo de salida
	jl salir				;Si es negativo no se pudo abrir el archivo por lo que debo mostrar el mensaje acorde y detener el programa
escrt:			;ESCRITURA DE ARCHIVO
	mov EBX,EAX				;muevo el descriptor de archivo a EBX para preparar la escritura.
	mov EAX,sys_write			;Quiero escribir en un archivo
	pop ECX					;lo que contiene este buffer
	mov EDX,BUFFSIZE			;la mayor cantidad de numeros que pueda hasta encontrar un EOF (caracter nulo), o tamaño maximo del buffer
	int 80h					;ejecuto la operacion sys_read, va a cargar el archivo en buffer
			;CIERRE DE ARCHIVO
	mov EAX,sys_close			;cierro el archivo
	pop EBX					;recupero nombre del archivo guardado 
	int 80h					;llamo a sys_close
	ret
;.......::::::: FIN GRABAR EN UN ARCHIVO UN BUFFER :::::::.......

;.......::::::: RUTINAS DE MANEJO DE ERRORES Y SALIDA :::::::.......
;ENTRADA:
	;En tope de pila el nivel de error.
;SALIDA:
	;Mensaje de error mostrado en pantalla.

salir:
	;Termina la ejecucion del programa con el nivel de error recibido por parametro
	;se determina que nivel sucedio y que mensaje mostrar 0<=EBX<=3.
	xor EBX, EBX		;limpio el registro EBX por las dudas
	pop EBX			;obtengo el nivel de error
	push EBX		;resguardo el nivel de error
	cmp EBX, 0		;comparo el nivel y cargo el mensaje correspondiente.
	jnz err1
	mov ECX, nerror
	mov EDX, lnerror
	jmp errfin
err1:
	cmp EBX, 1
	jnz err2
	mov ECX, error1
	mov EDX, lerror1
	jmp errfin
err2:
	cmp EBX, 2
	jnz err3
	mov ECX, error2
	mov EDX, lerror2
	jmp errfin
err3:
	cmp EBX, 3
	mov ECX, error3
	mov EDX, lerror3
errfin:				;se determino que error era, se seteo su mensaje en ECX y su largo en EDX, se llama a la rutina imprimir y se interrumpe el sistema con sys_exit
	call imprimir
	mov eax, sys_exit
	pop EBX			;obtengo el nivel de error resguardado (la rutina imprimir usa EBX para imprimir, le asigna 1).
	int 0x80
;.......::::::: FIN RUTINAS DE MANEJO DE ERRORES Y SALIDA :::::::.......