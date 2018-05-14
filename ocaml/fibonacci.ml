let fibby t1 t2 n = 
	let m = Array.make n (None : int option) in
	Array.set m 0 (Some t1);
	Array.set m 1 (Some t2);
	let rec fibbier p = match (Array.get m (p-1)) with
		| None -> let ans = (fibbier (p-2)) + (fibbier (p-1)) * (fibbier (p-1)) in
				Array.set m (p-1) (Some ans);
				ans;
		| Some q -> q in
		fibbier n;;
