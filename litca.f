	program litca
	real buf(84,84)
	integer fnom,fstlir,newdate,fstouv
	external fnom,fstlir,newdate,fstouv

	integer datev,dat2,dat3
	ier = fnom(10,'fichier89','std+rnd',0)

	ier = fstouv(10,'RND')
        ier=newdate(datev,19880730,04073000,3) 
        write(6,*)'datev ',datev 
        ier=newdate(datev,dat2,dat3,-3) 
   
c 
        ier=fstlir(buf,10,NI,NJ,NK,datev,' ',12091,-1,-1,' ','UU') 
        write(6,*)'ier = ',ier,dat2,dat3 
        write(6,*)'point= ',buf(20,20)

	ier = fstfrm(10)
	stop
	end
