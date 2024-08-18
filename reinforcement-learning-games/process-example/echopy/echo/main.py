import sys

def main():
    # print("Starting echo")
    keepGoing = True
    while keepGoing: 
        try:
            req = input()
            if req != "exit":
                resp = f"Got message: {req}"
                print(resp, end='\n', file=sys.stdout, flush=True)
            else:
                keepGoing = False
        except EOFError as e:
            keepGoing = False
    # print("Exiting...")

if __name__ == "__main__":
    main()

