


data Block = [Command]
data Command = Assign {name::String, val::Int} | While Command | Return  