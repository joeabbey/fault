using System;
using System.Collections.Generic;
using System.Linq;
using static System.Math;
using Alias = System.String;
global using System.Threading.Tasks;

namespace SampleApp.Models;

/// <summary>
/// A sample class for testing the parser.
/// </summary>
public sealed class Sample : BaseClass, IConfigurable
{
    private string _name;
    public static readonly int MaxSize = 100;
    protected List<string> Items { get; set; }

    public string Name { get; private set; }

    public Sample(string name)
    {
        _name = name;
        Items = new List<string>();
    }

    public string GetName()
    {
        return _name;
    }

    public void SetName(string name)
    {
        _name = name;
    }

    private int Calculate(int a, int b)
    {
        return a + b;
    }

    public static List<string> GetDefaultItems()
    {
        return new List<string>();
    }

    public async Task<string> FetchDataAsync()
    {
        return await Task.FromResult("data");
    }

    public override string ToString()
    {
        return $"Sample{{name={_name}}}";
    }
}

internal interface IConfigurable
{
    void Configure(string key, string value);
}

public enum Color
{
    Red,
    Green,
    Blue
}

public record Point(int X, int Y);

public abstract class AbstractService
{
    public abstract void Execute();
    public virtual void Log(string message) { }
}

public partial class PartialWidget
{
    public int Width { get; set; }
}

public struct Coordinate
{
    public double Latitude { get; set; }
    public double Longitude { get; set; }
}
