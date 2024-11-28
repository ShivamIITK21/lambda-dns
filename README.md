# lambda-dns

A recursive DNS resolver built in Haskell.

## Build 
```
stack build
```

## Run
May require superuser permissions for running on port 53,
```
stack exec lambda-dns
```

## Test
Send DNS requests to the server using dig, 
```
dig -p 53 @127.0.0.1 +noedns www.google.com    
```

