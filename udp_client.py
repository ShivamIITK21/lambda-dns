import socket
import sys

def udp_client(input_file, output_file, ip, port, timeout=5):
    try:
        # Create a UDP socket
        client_socket = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
        client_socket.settimeout(timeout)

        # Read the input file
        with open(input_file, "rb") as f:
            data = f.read()

        # Send the data to the server
        client_socket.sendto(data, (ip, port))
        print(f"Sent data from {input_file} to {ip}:{port}")

        # Wait for the response
        try:
            response, server = client_socket.recvfrom(4096)  # Buffer size of 4096 bytes
            print(f"Received response from {server}")

            # Write the response to the output file
            with open(output_file, "wb") as f:
                f.write(response)
            print(f"Response written to {output_file}")

        except socket.timeout:
            print("No response received: timeout occurred.")

    except FileNotFoundError:
        print(f"Error: File {input_file} not found.")
    except Exception as e:
        print(f"An error occurred: {e}")
    finally:
        # Close the socket
        client_socket.close()

if __name__ == "__main__":
    # Command-line arguments: input_file output_file ip port
    if len(sys.argv) != 5:
        print("Usage: python udp_client.py <input_file> <output_file> <ip> <port>")
        sys.exit(1)

    input_file = sys.argv[1]
    output_file = sys.argv[2]
    ip = sys.argv[3]
    port = int(sys.argv[4])

    
    udp_client(input_file, output_file, ip, port)

