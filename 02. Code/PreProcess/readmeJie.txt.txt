main.R is using parallel running on just TestNeg part

main_v3.r is using parallel running on the whole simulation process

main_v2.r is using parallel running on the whole simulation process, and what's more, it using parallel using on TestNeg part
as well. But due to the time-consuming in sfExport(dataset1, dataset2, ....) during TestNeg part. the main_v3.r is not faster
than main_v2.R. what's maybe a little bit strange.

So I remmended using main_v3.R, but not main_v2.R