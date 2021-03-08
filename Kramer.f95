! Muallif: Ashurov Sindor
! versiya: 1.0.1
! Ushbu dastur
! a_11*x1 + a_12*x2 + a_13*x3 = b1
! a_21*x1 + a_22*x2 + a_23*x3 = b2
! a_31*x1 + a_32*x2 + a_33*x3 = b3
! ko`rinishidagi uch noma'lumli, birinchi tartibli, bir jinsli tenglamalar sistemasini 
! Kramer usulida yechishga mo`ljallangan
! Ya'ni biz to`rtta determinant hisoblaymiz.
! Bu determinantlarni det, detX, detY va detZ deb belgilaymiz.


IMPLICIT NONE
REAL(8),DIMENSION(3,3)::a     !(3 x 3) 9 ta elementdan iborat bo`lgan ikki o`lchamli matritsa hosil qilamiz. 
! Bu matritsa tenglamalar sistemasining koeffitsiyentlari uchun

REAL(8),DIMENSION(3)::b,x  ! uchta elementdan iborat bo`lgan bir o`lchamli matritsa
REAL(8)::det,detX,detY,detZ  !determinantlar

WRITE(*,*)"Tenglamaning koeffitsientlarini kiriting:"
WRITE(*,*)"ESLATMA!"
WRITE(*,*)"Siz kiritgan koeffitsientlar a11,a12,a13,a21,a22,a23,a31,a32,a33 va b1,b2,b3 tartibida o`zlashtiriladi"

READ(*,*)a(1,1),a(1,2),a(1,3),a(2,1),a(2,2),a(2,3),a(3,1),a(3,2),a(3,3),b(1),b(2),b(3)

! Sistemaning asosiy determinantini hisoblaymiz:
! |a11    a12    a13|
! |a21    a22    a23|
! |a31    a32    a33|
! 
! Eslatma!
! Agar sistemaning asosiy determinanti nolga teng bo`lsa, sistema yechimga ega bo`lmaydi.
! Ya'ni sistemada qatnashgan tenglamalar uchta o`zaro parallel bo`lgan to`g`ri chiziqlar tenglamasi bo`ladi.

det=(a(1,1)*a(2,2)*a(3,3)+a(1,2)*a(2,3)*a(3,1)+a(1,3)*a(2,1)*a(3,2))-a(1,3)*a(2,2)*a(3,1)-a(1,1)*a(2,3)*a(3,2)-a(1,2)*a(2,1)*a(3,3)

! Keyin x ga bog`liq bo`lgan determinant:

! |b1    a12    a13|
! |b2    a22    a23|
! |b3    a32    a33|

detX=(b(1)*a(2,2)*a(3,3)+a(1,2)*a(2,3)*b(3)+a(1,3)*b(2)*a(3,2))-(a(1,3)*a(2,2)*b(3)+b(1)*a(2,3)*a(3,2)+a(1,2)*b(2)*a(3,3))

! y ga bog`liq bo`lgan determinant:
! |a11    b1    a13|
! |a21    b2    a23|
! |a31    b3    a33|

detY=(a(1,1)*b(2)*a(3,3)+b(1)*a(2,3)*a(3,1)+a(1,3)*a(2,1)*b(3))-(a(1,3)*b(2)*a(3,1)+a(1,1)*a(2,3)*b(3)+b(1)*a(2,1)*a(3,3))

! z ga bog`liq bo`lgan determinant:
! |a11    a12    b1|
! |a21    a22    b2|
! |a31    a32    b3|
detZ=(a(1,1)*a(2,2)*b(3)+a(1,2)*b(2)*a(3,1)+b(1)*a(2,1)*a(3,2))-(b(1)*a(2,2)*a(3,1)+a(1,1)*b(2)*a(3,2)+a(1,2)*a(2,1)*b(3))

! Shundan so`ng tenglamaning x1, x2 va x3 ildizlarini topish mumkin:
x(1)=detX/det
x(2)=detY/det
x(3)=detZ/det

! Natijani ekranga chiqaramiz:
WRITE(*,*)x(1),x(2),x(3)
END
