goldenRatio = (1+math.sqrt(5))/2
invphi = (math.sqrt(5) - 1) / 2 # 1/phi                                                                                                                     
invphi2 = (3 - math.sqrt(5)) / 2 # 1/phi^2                                                                                                                  

def gssrec(f,a,b,tol=1e-5,h=None,c=None,d=None,fc=None,fd=None):
    '''                                                                                                                                                     
    Golden section search, recursive.                                                                                                                           
                                                                                                                                                            
    Given a function f with a single local minimum in                                                                                                       
    the interval [a,b], gss returns a subset interval                                                                                                       
    [c,d] that contains the minimum with d-c <= tol.                                                                                                        
                                                                                                                                                            
    example:                                                                                                                                                
    >>> f = lambda x: (x-2)**2                                                                                                                              
    >>> a = 1                                                                                                                                               
    >>> b = 5                                                                                                                                               
    >>> tol = 1e-5                                                                                                                                          
    >>> (c,d) = gssrec(f, a, b, tol)                                                                                                                           
    >>> print (c,d)                                                                                                                                         
    (1.9999959837979107, 2.0000050911830893)                                                                                                                
    '''
    
    (a,b)=(min(a,b),max(a,b))
    if h == None: h=b-a
    if h <= tol: return (a,b)
    if c == None: c = a + invphi2*h
    if d == None: d = a + invphi*h
    if fc == None: fc = f(c)
    if fd == None: fd = f(d)
    if fc < fd:
        return gssrec(f,a,d,tol,h*invphi,c=None,fc=None,d=c,fd=fc)
    else:
        return gssrec(f,c,b,tol,h*invphi,c=d,fc=fd,d=None,fd=None)

def test5():