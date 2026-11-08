#include <iostream>
#include <vector>
#include <algorithm>
#include <unordered_map>
#include <optional>
#include <stdexcept>
#include <string>
#include <iomanip>
#include <cctype>

using namespace std;

// Convert HH:MM into minutes
int convertToMinutes(const string& time) 
{

    if (time.length() != 5 || time[2] != ':')
        throw invalid_argument("Use HH:MM format!");

    if (!isdigit(time[0]) || !isdigit(time[1]) ||
        !isdigit(time[3]) || !isdigit(time[4]))
        throw invalid_argument("Invalid time format!");

    int hours = stoi(time.substr(0, 2));
    int minutes = stoi(time.substr(3, 2));

    if (hours > 23 || minutes > 59)
        throw invalid_argument("Invalid clock time!");

    return hours * 60 + minutes;
}

// Convert minutes into readable clock time
string convertToTime(int totalMinutes) 
{

    int day = totalMinutes / 1440;
    int timeInDay = totalMinutes % 1440;

    int hours = timeInDay / 60;
    int minutes = timeInDay % 60;

    string result =
        (hours < 10 ? "0" : "") + to_string(hours) + ":" +
        (minutes < 10 ? "0" : "") + to_string(minutes);

    if (day > 0)
        result += " (+" + to_string(day) + " day)";

    return result;
}

// Patient structure
struct Patient 
{
    int id;
    int severity;
    int arrivalTime;
    int treatmentDuration;
    long long sequence;
};

// Custom Max Heap
class TriageQueue
{

public:

    bool empty() const 
	{
        return heap.empty();
    }

    void insert(Patient patient) 
	{

        if (indexById.count(patient.id))
            throw invalid_argument("Duplicate Patient ID!");

        if (patient.severity < 0 ||
            patient.arrivalTime < 0 ||
            patient.treatmentDuration <= 0)
            throw invalid_argument("Invalid patient details!");

        patient.sequence = nextSequence++;

        heap.push_back(patient);

        indexById[patient.id] = heap.size() - 1;

        siftUp(heap.size() - 1);
    }

    optional<Patient> peek() const 
	{

        if (heap.empty())
            return nullopt;

        return heap.front();
    }

    optional<Patient> extractMax() 
	{

        if (heap.empty())
            return nullopt;

        Patient result = heap.front();

        indexById.erase(result.id);

        if (heap.size() == 1) 
		{
            heap.pop_back();
            return result;
        }

        heap.front() = heap.back();

        heap.pop_back();

        indexById[heap.front().id] = 0;

        siftDown(0);

        return result;
    }

    bool updateSeverity(int id, int severity) 
	{

        if (severity < 0)
            throw invalid_argument("Severity cannot be negative!");

        auto it = indexById.find(id);

        if (it == indexById.end())
            return false;

        size_t i = it->second;

        heap[i].severity = severity;

        if (i > 0 && higherPriority(heap[i], heap[parent(i)]))
            siftUp(i);
        else
            siftDown(i);

        return true;
    }

private:

    vector<Patient> heap;

    unordered_map<int, size_t> indexById;

    long long nextSequence = 0;

    static size_t parent(size_t i) 
	{
        return (i - 1) / 2;
    }

    // Priority comparison
    static bool higherPriority(
        const Patient& a,
        const Patient& b) 
	{

        if (a.severity != b.severity)
            return a.severity > b.severity;

        if (a.arrivalTime != b.arrivalTime)
            return a.arrivalTime < b.arrivalTime;

        return a.sequence < b.sequence;
    }

    void swapPatients(size_t a, size_t b) 
	{

        swap(heap[a], heap[b]);

        indexById[heap[a].id] = a;

        indexById[heap[b].id] = b;
    }

    // Move element upward
    void siftUp(size_t i) 
	{

        while (i > 0) 
		{

            size_t p = parent(i);

            if (!higherPriority(heap[i], heap[p]))
                break;

            swapPatients(i, p);

            i = p;
        }
    }

    // Move element downward
    void siftDown(size_t i) 
	{

        while (true) 
		{

            size_t best = i;

            size_t left = 2 * i + 1;

            size_t right = 2 * i + 2;

            if (left < heap.size() &&
                higherPriority(heap[left], heap[best]))
                best = left;

            if (right < heap.size() &&
                higherPriority(heap[right], heap[best]))
                best = right;

            if (best == i)
                break;

            swapPatients(i, best);

            i = best;
        }
    }
};

// Treatment record
struct TreatmentRecord 
{

    Patient patient;

    int startTime;

    int waitingTime;

    int completionTime;
};

// Patient treatment simulation
vector<TreatmentRecord> simulate(vector<Patient> arrivals) 
{

    stable_sort
	(
        arrivals.begin(),
        arrivals.end(),
        [](const Patient& a, const Patient& b) 
		{

            return a.arrivalTime < b.arrivalTime;
        }
    );

    TriageQueue queue;

    vector<TreatmentRecord> records;

    size_t nextArrival = 0;

    int currentTime = 0;

    while (nextArrival < arrivals.size() || !queue.empty()) 
	{

        // Move time to next arrival if queue is empty
        if (queue.empty() &&
            currentTime < arrivals[nextArrival].arrivalTime) 
		{

            currentTime = arrivals[nextArrival].arrivalTime;
        }

        // Insert all arrived patients
        while (nextArrival < arrivals.size() &&
               arrivals[nextArrival].arrivalTime <= currentTime) 
		{

            queue.insert(arrivals[nextArrival]);

            nextArrival++;
        }

        // Select highest priority patient
        auto nextPatient = queue.extractMax();

        if (!nextPatient)
            break;

        int waitingTime =
            currentTime - nextPatient->arrivalTime;

        int completionTime =
            currentTime + nextPatient->treatmentDuration;

        records.push_back
		({
            *nextPatient,
            currentTime,
            waitingTime,
            completionTime
        });

        // Update current time
        currentTime = completionTime;
    }

    return records;
}

// Main function
int main() 
{

    int n;

    cout << "====================================\n";

    cout << "   EMERGENCY ROOM TRIAGE SYSTEM\n";

    cout << "====================================\n";

    cout << "\nEnter number of patients: ";

    if (!(cin >> n) || n <= 0)
	{

        cout << "Invalid number of patients!\n";

        return 1;
    }

    vector<Patient> arrivals;

    try 
	{

        cout << "\nEnter Patient Details:\n";

        for (int i = 0; i < n; i++) 
		{

            Patient p{};

            string arrival;

            cout << "\nPatient " << i + 1 << "\n";

            cout << "Patient ID: ";

            if (!(cin >> p.id))
                throw invalid_argument("Invalid Patient ID!");

            cout << "Severity (higher value = more urgent): ";

            if (!(cin >> p.severity) || p.severity < 0)
                throw invalid_argument("Invalid severity!");

            cout << "Arrival Time (HH:MM): ";

            if (!(cin >> arrival))
                throw invalid_argument("Invalid arrival time!");

            p.arrivalTime = convertToMinutes(arrival);

            cout << "Treatment Duration (minutes): ";

            if (!(cin >> p.treatmentDuration) ||
                p.treatmentDuration <= 0)
                throw invalid_argument("Invalid treatment duration!");

            p.sequence = 0;

            arrivals.push_back(p);
        }

        // Simulate treatment
        vector<TreatmentRecord> records = simulate(arrivals);

        // Display results
        cout << "\n====================================\n";

        cout << " TREATMENT ORDER AND WAITING TIMES\n";

        cout << "====================================\n";

        for (const auto& r : records) 
		{

            cout << "\nPatient ID: "
                 << r.patient.id;

            cout << "\nSeverity: "
                 << r.patient.severity;

            cout << "\nArrival Time: "
                 << convertToTime(r.patient.arrivalTime);

            cout << "\nTreatment Start Time: "
                 << convertToTime(r.startTime);

            cout << "\nTreatment Duration: "
                 << r.patient.treatmentDuration
                 << " minutes";

            cout << "\nWaiting Time: "
                 << r.waitingTime
                 << " minutes";

            cout << "\nCompletion Time: "
                 << convertToTime(r.completionTime);

            cout << "\n";
        }

        cout << "\n====================================\n";

        cout << "All patients processed successfully!\n";

        cout << "====================================\n";

    }

    catch (const exception& e) 
	{

        cout << "\nError: " << e.what() << "\n";

        return 1;
    }

    return 0;
}