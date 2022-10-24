program testquad

integer, parameter :: QPK = selected_real_kind (16)
integer, parameter :: DPK = selected_real_kind (8)
integer, parameter :: SPK = selected_real_kind (4)

real (kind=QPK) :: qtmpi, qpi, qj, qf, refpi
real (kind=DPK) :: dtmpi, dpi, dj, df
real (kind=SPK) :: stmpi, spi, sj, sf
integer (kind=8) :: j, endj

endj = 2000000000
refpi = 3.1415926535897932384626433

qf = 4.0_16
df = 4.0_8
sf = 4.0_4

qpi = qf
dpi = df
spi = sf

qj = 3.0_16
dj = 3.0_8
sj = 3.0_4

do i=1,endj
  qtmpi = qf/qj
  dtmpi = df/dj
  stmpi = sf/sj
  if (mod(i,2) .ne. 0) then 
    qpi = qpi - qtmpi
    dpi = dpi - dtmpi
    spi = spi - stmpi
  else
    qpi = qpi + qtmpi
    dpi = dpi + dtmpi
    spi = spi + stmpi
  endif
  qj = qj + 2.0_16
  dj = dj + 2.0_8
  sj = sj + 2.0_4
enddo

print *, "Quad: ", qpi, refpi-qpi
print *, "Doub: ", dpi, refpi-dpi
print *, "Sing: ", spi, refpi-spi

end program

